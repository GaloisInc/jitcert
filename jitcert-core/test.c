// This is a C-ish file for testing the proof-of-concept YAML scraper

//-| - testAnnotation: &testTests
//-|     tsFeatures:
//-|       - "PRNG"
//-|     tsEnvironment: "Linux"
bool testFoo() {
  int x = foo();
  return x == 3;
}

//-| - modAnnotation: &testModule
//-|     modName: "TestModule"
//-|     modLanguage: "C"
//-|     modAuthors:
//-|       - "Karl Smeltzer"
//-|     modVersion: 42
//-|     modBoundary: 
//-|       - "cryptographic"
//-|       - "module"

// The idea of a software annotation is to describe a collection of modules.
// Normally it woudl probably be in a different file, but that requires more
// programming to prevent anchor ordering from mattering

//-| - swAnnotation:
//-|     swName: "My Application"
//-|     swOS: "Linux"
//-|     swModules: 
//-|       - *testModule
//-|     swFiles:
//-|       - "test.c"
//-|     swTestSuites: 
//-|       - *testTests


//-| - fnAnnotation:
//-|     fnName: "foo"
//-|     fnFeature: "PRNG"
//-|     fnParameters: []
//-|     fnReturn: "void"
//-|     fnCalls: []
//-|     fnTestSuite: *testTests
int foo() {
  //-| - lineAnnotation:
  //-|     lnFeature: "PRNG"
  //-|     lnNumber: 47
  return 1 + 2;
}

