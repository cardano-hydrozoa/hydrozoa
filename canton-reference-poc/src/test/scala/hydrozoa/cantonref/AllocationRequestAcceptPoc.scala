package hydrozoa.cantonref

import cats.data.StateT
import com.digitalasset.daml.lf.command.ApiCommand
import com.digitalasset.daml.lf.data.Ref
import daml.splice.api.token.allocationinstructionv2.AllocationFactory
import daml.splice.api.token.allocationinstructionv2.AllocationInstruction
import daml.splice.api.token.allocationinstructionv2.AllocationInstruction_Accept
import daml.splice.api.token.allocationinstructionv2.AllocationInstructionResult
import daml.splice.api.token.allocationinstructionv2.allocationinstructionresult_output.AllocationInstructionResult_Completed
import daml.splice.api.token.allocationinstructionv2.allocationinstructionresult_output.AllocationInstructionResult_Pending
import daml.splice.api.token.allocationv2.Allocation
import daml.splice.api.token.holdingv2.Account
import daml.splice.api.token.holdingv2.HoldingView
import daml.splice.api.token.holdingv2.InstrumentId
import daml.splice.api.token.metadatav1.ExtraArgs
import daml.splice.testing.tokens.testtokenv2.accountconfig.AccountConfig
import daml.splice.testing.tokens.testtokenv2.accountconfig.PartyConfig
import daml.splice.testing.tokens.testtokenv2.holding.Token
import org.scalatest.funsuite.AnyFunSuite
import tokenstandard.PartyId
import tokenstandard.TokenStandardHelpers
import tokenstandard.TokenStandardHelpers.basicAccount
import tokenstandard.engine.DamlEngine
import tokenstandard.engine.EngineLedger
import tokenstandard.engine.EngineM
import tokenstandard.engine.EngineRegistry
import tokenstandard.engine.EngineStore
import tokenstandard.registry.RegistryApi.Error

import java.time.Instant
import java.util.Optional
import scala.jdk.CollectionConverters.*

/** PoC (consuming the canton-reference-registry engine tier): alice and bob deposit into a treasury
  * via the **allocation request + accept** two-step. Their accounts require the provider to
  * approve, so each holder's `AllocationFactory_Allocate` returns a *pending*
  * `AllocationInstruction` (the request), and the provider then exercises
  * `AllocationInstruction_Accept` — its choice context assembled by the reference registry's
  * `getAllocationInstructionAcceptContext` — to finalize the allocation. Runs in-process against
  * the real Daml interpreter; no Canton.
  */
class AllocationRequestAcceptPoc extends AnyFunSuite:

    private val reg = PartyId("reg")
    private val provider = PartyId("provider")
    private val alice = PartyId("alice")
    private val bob = PartyId("bob")
    private val operator = PartyId("operator")

    private val engine = DamlEngine.load()
    private val ledger = new EngineLedger(engine)
    // one registry admin serves both instruments
    private val reference = EngineRegistry(engine, reg, List("X", "Y"))

    /** A configured account whose provider must approve token-standard actions (the two-step
      * trigger). Owner + provider are the `Account`/`AccountConfig` signatories.
      */
    private def cfgAccount(owner: PartyId): Account =
        new Account(Optional.of(owner.value), Optional.of(provider.value), "cfg")

    test("alice + bob deposit via allocation request + provider accept"):
        val xId = new InstrumentId(reg.value, "X")
        val yId = new InstrumentId(reg.value, "Y")
        val settlement = TokenStandardHelpers.settlementInfo(List(operator), "treasury/deposit")

        val program: EngineM[(BigDecimal, BigDecimal, BigDecimal, BigDecimal)] =
            for
                _ <- ledger.createTokenRules(reg)
                _ <- seedConfigured(alice, xId, BigDecimal(1000))
                _ <- seedConfigured(bob, yId, BigDecimal(1000))

                // each holder REQUESTS a sender-side deposit allocation, then the provider ACCEPTS.
                _ <- requestAndAccept(alice, cfgAccount(alice), xId, BigDecimal(100), settlement)
                _ <- requestAndAccept(bob, cfgAccount(bob), yId, BigDecimal(100), settlement)

                aliceUnlocked <- ledger.unlockedBalance(alice, alice, xId)
                aliceLocked <- ledger.lockedBalance(alice, alice, xId)
                bobUnlocked <- ledger.unlockedBalance(bob, bob, yId)
                bobLocked <- ledger.lockedBalance(bob, bob, yId)
            yield (aliceUnlocked, aliceLocked, bobUnlocked, bobLocked)

        program.run(EngineStore.empty) match
            case Right((_, (aU, aL, bU, bL))) =>
                // each holder's 100 is now locked behind its accepted deposit allocation.
                assert(
                  aL == BigDecimal(100) && aU == BigDecimal(900) &&
                      bL == BigDecimal(100) && bU == BigDecimal(900),
                  s"balances aliceLocked=$aL aliceUnlocked=$aU bobLocked=$bL bobUnlocked=$bU " +
                      s"(want 100/900/100/900)",
                )
            case Left(e) => fail(s"request+accept flow failed: $e")

    /** Request a sender-side allocation (`AllocationFactory_Allocate`, actor = owner) — which goes
      * *pending* because the provider must still approve — then accept it as the provider
      * (`AllocationInstruction_Accept`), returning the finalized allocation.
      */
    private def requestAndAccept(
        owner: PartyId,
        authorizer: Account,
        instrument: InstrumentId,
        amount: BigDecimal,
        settlement: daml.splice.api.token.allocationv2.SettlementInfo,
    ): EngineM[Allocation.ContractId] =
        val leg = TokenStandardHelpers.transferLeg(
          s"${owner.value}-deposit",
          authorizer,
          operator.basicAccount,
          amount,
          instrument.id,
        )
        val spec = TokenStandardHelpers.allocationSpec(
          reg,
          authorizer,
          List(TokenStandardHelpers.senderSide(leg)),
          committed = false,
          None,
        )
        for
            inputs <- ledger.listHoldingCids(owner, owner, instrument)
            bundle <- reference.getAllocationFactory(
              TokenStandardHelpers.allocationFactoryAllocate(
                settlement,
                spec,
                Instant.EPOCH,
                inputs,
                List(owner),
              )
            )
            // the REQUEST — owner exercises the factory; provider hasn't approved, so it is pending
            requested <- ledger.exercise(
              List(owner),
              Nil,
              new AllocationFactory.ContractId(bundle.factoryCid)
                  .exerciseAllocationFactory_Allocate(bundle.arg),
              bundle.disclosures,
            )
            instrCid <- liftEither(pendingInstruction(requested.exerciseResult))
            // the ACCEPT — provider exercises the pending instruction with the registry's context
            ctx <- reference.getAllocationInstructionAcceptContext(
              instrCid,
              TokenStandardHelpers.emptyMetadata,
            )
            accepted <- ledger.exercise(
              List(provider),
              Nil,
              instrCid.exerciseAllocationInstruction_Accept(
                new AllocationInstruction_Accept(
                  List(provider.value).asJava,
                  new ExtraArgs(ctx.choiceContext, TokenStandardHelpers.emptyMetadata),
                )
              ),
              ctx.disclosures,
            )
            cid <- liftEither(completedAllocation(accepted.exerciseResult))
        yield cid

    /** Seed a provider-`mustApprove` `AccountConfig` and a `Token` holding for `owner`, created
      * directly through the engine (both signed by owner + provider; the Token also by admin). The
      * library's `EngineLedger.seedHolding` only makes basic accounts, so we build the create
      * commands here — a PoC setup convenience, not the reference API.
      */
    private def seedConfigured(
        owner: PartyId,
        instrument: InstrumentId,
        amount: BigDecimal,
    ): EngineM[Unit] =
        val account = cfgAccount(owner)
        val config = new AccountConfig(
          reg.value,
          account,
          new PartyConfig(true, true), // owner: can initiate, must approve
          new PartyConfig(false, true), // provider: must approve (the two-step trigger)
        )
        val token = new Token(
          new HoldingView(
            account,
            instrument,
            amount.bigDecimal,
            Optional.empty(),
            TokenStandardHelpers.emptyMetadata,
          )
        )
        val submitters = Set(reg, owner, provider).map(p => Ref.Party.assertFromString(p.value))
        val creates = List(
          ApiCommand
              .Create(engine.toTypeConRef(AccountConfig.TEMPLATE_ID), engine.toLf(config.toValue)),
          ApiCommand.Create(engine.toTypeConRef(Token.TEMPLATE_ID), engine.toLf(token.toValue)),
        )
        engine.submit(submitters, creates).map(_ => ())

    private def pendingInstruction(
        res: AllocationInstructionResult
    ): Either[Error, AllocationInstruction.ContractId] =
        res.output match
            case p: AllocationInstructionResult_Pending => Right(p.allocationInstructionCid)
            case other => Left(Error.Unexpected(s"expected a pending allocation, got: $other"))

    private def completedAllocation(
        res: AllocationInstructionResult
    ): Either[Error, Allocation.ContractId] =
        res.output match
            case c: AllocationInstructionResult_Completed => Right(c.allocationCid)
            case other => Left(Error.Unexpected(s"accept did not complete the allocation: $other"))

    private def liftEither[A](e: Either[Error, A]): EngineM[A] =
        StateT.liftF[[X] =>> Either[Error, X], EngineStore, A](e)
