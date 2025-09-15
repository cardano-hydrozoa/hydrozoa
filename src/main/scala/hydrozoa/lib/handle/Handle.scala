package hydrozoa.lib.handle

object Handle {
    trait RequestAsync

    trait RequestSync extends RequestAsync {
        type Response
    }

    trait HandleAsync[F[+_], -Request <: RequestAsync] {
        def !(req: Request): F[Unit]
    }

    trait HandleOnlySync[F[+_], -Request <: RequestSync] {
        def ?(request: Request): F[request.Response]
    }

    trait Handle[F[+_], -RequestA <: RequestAsync, -RequestS <: RequestSync]
        extends HandleAsync[F, RequestA],
          HandleOnlySync[F, RequestS]
}
