function genXs(){
  xs = dnorm(traceValues[1], MSMus, sigma) * sigma * traceValues[1]
}