# MUPL and Resil languages

This folder contains SML codes that build MUPL and RSL languages.
It corresponds to the second and last part of the extra problems of the section/week 6 of Coursera PL. (The first part was implemented in Racket which could be found [here](https://gist.github.com/kokoro-aya/961bbd987a2604093873e40ced35bdac))

The second part is a porting of MUPL implementation into SML environment.
The following files are used for this purpose:

- signatures.sml  -- Definition of MUPL data types and methods
- env.sig.sml     -- Definition of ENV data types and methods that's mixin-ed into MUPL
- envtest.sml     -- Test suite for a simple implementation of ENV
- mupl.sml        -- Implementation of MUPL Evaluator
- mupltest.sml    -- Test suites for this evaluator

The last part is an extension of MUPL into Resil. Sig and data types have been adapted following directions of the [coursera pl extra assignments](https://github.com/edombowsky/coursera-pl/blob/master/assignments/section6/ExtraProblems.md).

I have added a minimum of new language structures for my purpose, like CallDyn and Str.

I am currently working with the last part of this assignment, i.e. the type checking. But a more extended version of the language is planned, to add new supports like pattern matching, records, etc.
