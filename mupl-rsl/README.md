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

- resil.sig.sml   -- Signatures for the extended Resil language
- resil.lang.sml  -- Language implementation
- resil.test.sml  -- Testing cases for Resil

- resil.typecheck.sml       -- Implementation of typecheck mechanism
- resil.typecheck.test.sml  -- Testing cases for typecheck feature

The last part is an extension of MUPL into Resil. Sig and data types have been adapted following directions of the [coursera pl extra assignments](https://github.com/edombowsky/coursera-pl/blob/master/assignments/section6/ExtraProblems.md).

I have added a minimum of new language structures for my purpose, like CallDyn and Str.

The typecheck feature should be roughly done, but developing this feature in pure SML environment is quite painful. Globally, the typecheck works for simple types like literals, variables, functions or tuples, with a limited usage of variables, like `fn x => fn y => x + y`. Some polymorphism works, like `fn f => fn x => f x`, but more complex functions like `val double = fn f => fn x => f f x` won't be properly inferred.

The further development of language and typing are planned on the Scala port since it's painful to develop in pure SML environment without any debugger or IDE support.

The code of typecheck was a bit messy as the explanation of typecheck in assignment was a bit blurry and the algorithm itself was not so straightforward.