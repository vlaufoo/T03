## Changes to Klessydra for synthesis on Vivado (VHDL 1993)

Changes made to the description of the core to aapt it to the 1993 standard will be marked with the comment "VHDL1993" they include:
- Changing of subtype definition, not possible in the ports section of an entity in the 1993 version. Since the definition of a subtype has to be put in a package or other declarative sections, like before beginning the description of an architecture, the solution proposed is to remove the generic "THREAD_POOL_SIZE" from the design and instead use the constant included in the klessydra package called "THREAD_POOL_BASELINE", which is otherwised unmentioned in other files.
- Reassignment of output ports to internal signals.
- Rewriting of processes' sensitivitylists, since they do not support the keyword 'all' in 1993 VHDL.

For te first problem I have decided to implement also a second, more complex but more versatile solution, that wraps all entities that need to use the subtype in wrapper entities, solely intended fro the purpose of defining the subtype in their architecture using (and passing on) the generic
"THREAD_POOL_SIZE", this will still allow to change the number of replicated components on a per-component basis, even though I see no use for it.
