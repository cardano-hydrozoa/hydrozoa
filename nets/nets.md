# Petri Net Library Documentation

WARNING: This is out of date as of 2026-05-14

- Topology[TVF]: 
  - This is only the "connectedness" of the different parts of the net. 
  - Building net topology requires creating "elements": transitions, nodes, and arcs. 
  - Topologies are _typed_. We require assigning unique IDs to each element, and those IDs are of a specific type.
  - Topologies have _builders_ that can create, update, and delete elements.
  - Topologies can be _valid_ or _invalid_. Invalid topologies have dangling arcs; valid topologies do not.
  - Topologies can be _added_ (creating two parallel nets) or _glued_ (identifiying two places in a net)
- Configuration[CVF] (extends Topology[TVF]): 
  - This associates "configuration data" with the topology.
  - It is _not_ the "marking" of the net
  - It specifies:
    - How many tokens are produced or consumed by a given arc
    - Place capacity
    - Final markings
    - DPN Data variables
    - Transition pre-conditions (guards)
    - Transition post-conditions
    - Arc Type? I'm not sure whether this needs to be in topology or configuration
  - Configurations can be _valid_ or _invalid_:
    - Valid configurations have:
      - Well-formed data variables and pre/post-condition expressions
      - Configuration associated with every element
  - Configurations have builders (configurators) that can set, modify, and delete configuration.
- Marking: 
  - Associated tokens with places.
  - Should we allow a marking to associate tokens to an invalid configuration or topology?
  - I think we could. But obviously the configuration could conflict, if the place is bounded.
- Simulator (extends Configuration):
  - This associates "enabledness" and "firing" methods with a configured net
    - getEnabledTransitions
    - fireTransition
    - fireTransitionUnsafe
    - isEnabled
    - autoFireTransitions
  - Question: Should we allow simulating a net with an invalid topology or configuration net if the simulation only affects some valid subnet?
    - I think we could. We can certainly extract T/C-valid nets from T/C-invalid nets -- even if its trivial (empty net). It's simply looking at the set of transitions with non-dangling arcs, such that each node has a configuration.
- Presentation:
  - This is the graphical representation of the net. 
  - position: (x,y)
  - Node (place, transition) size
  - arc path
  - whether a transition is silent
  - (graphical) delay for auto-firing
  - labels (distinct from element IDs -- can be duplicate)
  - other application-specific presentation data like stylesheets








