## Changes to Klessydra for synthesis on Vivado (VHDL 1993)

Changes made to the description of the core, to adapt it to the 1993 standard, will be marked with the comment "VHDL1993". They include:
- Changing of subtype definition, not possible in the ports section of an entity in the 1993 version. This can be done iwther by creating wrapper entities for eache ntity that needs them, and defining the subtype in the wrappe, or changing the way the hardware threads are implemented, fixing the
  number in the package as a constant and discarding the generic THREAD_POOL_SIZE that is passed between components, making it possible to define the subtype together with the type (or even define each subtype needed as a type of its own).
- Reassignment of output ports to internal signals.
- Rewriting of processes' sensitivitylists, since they do not support the keyword 'all' in 1993 VHDL.

### Redefinition of array types
A subtype cannot be defined as it is being used in the definition of a port, in the 1993 standard of VHDL.
In order to use the array types implemented to replicate harc units and to create the register file, the subtype corresponding to each of these applications has to be defined before the subtype is used in an entity.
The easiest way is implemented inthis branch, redefining subtypes for all applications in the klessydra package directly.
