- Try different methods to propagate the expected value,
  i.e. consider the expected non-trivial size bounds method. 
  Don't only compute size bounds by taking the expected runtime and multiplying this with the var
  change but also consider the expected var change and non-probabilistic runtime, 
  and check if the expected time multiplied with the expected var change is concave/convexe
- Factor in upper bounds on the runtime in expected local size bounds
