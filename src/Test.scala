import scala.collection.parallel.mutable.ParArray
import pso.fitness.FitnessFunction
import pso.core.PSOUtils
import pso.halt.HaltConditionIterations
import pso.core.PSODefault
import pso.core.Parameters
import pso.variants.dwpso.PSODW
import pso.core.PSODefault
import pso.variants.mopso.ManyFitnessFunctions
import pso.variants.mopso.PSOMultiObjective

object Test {
  
  
  def main(args: Array[String]): Unit = {
    val multiFuncs: ParArray[FitnessFunction] = ParArray(MultiA, MultiB)
    val multiFunc: ManyFitnessFunctions = new ManyFitnessFunctions(multiFuncs)
    
    val pars: Parameters = new Parameters(MultiA, new HaltConditionIterations(1000), 100)
    val pso: PSOMultiObjective = new PSOMultiObjective(pars, multiFunc)
    pso.runPSO()
  }
  
  /*Note that all the results are multiplied by -1. This is because these functions are specified with global
   * minimums and our PSO implementation looks for maximums
   */
  
  object Sphere extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      -solution.par.foldLeft(0.0)(_ + Math.pow(_, 2)) / solution.length
    }
  }
  
  object Rastrigin extends FitnessFunction {
    val A: Double = 10
    override def getFitness(solution: ParArray[Double]): Double = {
      val constant = A * solution.length
      val results = solution.par.map({
        case x => x * x - A * math.cos(2 * math.Pi * x)})
      -(constant + results.par.reduceLeft(_+_))
    }
  }
  
  object Rosenbrock extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      //xs refers to xi
      val xs = solution.take(solution.length - 1)
      //ys refer to xi+1
      val ys = solution.tail
      val pairs = xs.zip(ys)

      val results = pairs.par.map({
        case (x,y) => 100 * math.pow(y - x * x, 2) + math.pow(x - 1, 2)})
      -results.par.reduceLeft(_+_)
    }
  }
  
  //http://www.robertmarks.org/Classes/ENGR5358/Papers/functions.pdf 2.2
  object AxisHyperEllipsoid extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      val range = ParArray.range(1, solution.length + 1)
      val pairs = range.zip(solution)
      val results = pairs.par.map({
        case (i,x) => i * x})
      -results.par.reduceLeft(_+_)
    }
  }
  
  //http://www.robertmarks.org/Classes/ENGR5358/Papers/functions.pdf 2.6
  object Schwefel extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      val results = solution.par.map({
        case x => -x * math.sin(math.sqrt(math.abs(x)))})
      -results.par.reduceLeft(_+_)
    }
  }
  
  //http://www.robertmarks.org/Classes/ENGR5358/Papers/functions.pdf 2.7
  object Griewangk extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      val range = ParArray.range(1, solution.length + 1)
      val pairs = range.zip(solution)
      val cosParts: ParArray[Double] = pairs.par.map({
        case (i,x) => math.cos(x / math.sqrt(i))})
      val squareParts = solution.par.map({
        case x => x * x})
      -(1/4000 * squareParts.par.reduceLeft(_+_) - cosParts.par.reduceLeft(_*_) + 1)
    }
  }
  
  //http://www.robertmarks.org/Classes/ENGR5358/Papers/functions.pdf 2.8
  object IncreasingPowerSum extends FitnessFunction {
    
    override def getFitness(solution: ParArray[Double]): Double = {
      val range = ParArray.range(1, solution.length + 1)
      val pairs = range.zip(solution)
      val results = pairs.par.map({
        case (i, x) => math.pow(math.abs(x), i + 1)})
      -results.par.reduceLeft(_+_)
    }
  }
  
  //http://www.robertmarks.org/Classes/ENGR5358/Papers/functions.pdf 2.9
  object Ackley extends FitnessFunction {
    
    val a: Double = 20
    val b: Double = 0.2
    val c: Double = 2 * math.Pi
    
    override def getFitness(solution: ParArray[Double]): Double = {
      val squares = solution.par.map({
        case x => x * x})
      val squareRoot = math.sqrt(squares.reduceLeft(_+_) / solution.length)
      val exponent1 = -a * math.exp(-b * squareRoot)
      val cosParts = solution.par.map({
        case x => math.cos(c * x)})
      val exponent2 = -math.exp(cosParts.reduceLeft(_+_) / solution.length)
      -(exponent1 + exponent2 + a + math.exp(1))
    }
  }
  
  //from Studio5 -- fitness1
  object MultiA extends FitnessFunction {
    
    def getFitness(solution: ParArray[Double]): Double = {
      solution.head
    }
  }
  
  //from studio5 -- fitness2
  object MultiB extends FitnessFunction {
    
    def getFitness(solution: ParArray[Double]): Double = {
      val rest = solution.tail
      val sum = rest.par.reduceLeft(_+_)
      val gx = 1 + 9 * sum / (solution.length - 1)
      
      val hx = 1 - math.sqrt(solution.head / gx)
      hx * gx
    }
  }
  
}