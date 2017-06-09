import scala.collection.parallel.mutable.ParArray
import pso.fitness.FitnessFunction
import pso.core.PSOUtils
import pso.halt.HaltConditionIterations
import pso.core.PSODefault
import pso.core.Parameters
import pso.variants.dwpso.PSODW
import pso.core.PSODefault
import pso.variants.levy._
import pso.variants.mopso._
import pso.variants.dwpso._
import pso.variants.sopso._
import pso.variants.tvpso._
import pso.variants.tvapso._

/**
 * An example implementation object for testing
 */
object Test {
  
  /**
   * Runs the test
   * 
   */
  def main(args: Array[String]): Unit = {
    
    // Default PSO
    val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 10)
    val pso: PSODefault = new PSODefault(pars)
    
    // Levy PSO
    //val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 30)
    //val pso: PSOLevy = new PSOLevy(pars)
    
    // Levy PSO
    //val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 30)
    //val pso: PSOLevy = new PSOLevy(pars)
    
    // Decreasing Weight PSO
    //val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 30)
    //val pso: PSODW = new PSODW(pars)
    
    // Self organizing PSO
    //val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 30)
    //val pso: PSOSelfOrg = new PSOSelfOrg(pars, 100)
    
    // Time variant PSO
    //val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 30)
    //val pso: PSOTimeVary = new PSOTimeVary(pars, 1.0, 1.0, 10000)
    
    // Time variant accelerated PSO
    //val pars: Parameters = new Parameters(Sphere, new HaltConditionIterations(10000), 30)
    //val pso: PSOTimeVaryAcc = new PSOTimeVaryAcc(pars, 1.0, 1.0, 10000)
    
    //val multiFuncs: ParArray[FitnessFunction] = ParArray(MultiA, MultiB)
    //val multiFunc: MultiFitnessFunction = new MultiFitnessFunction(multiFuncs)
    //val pars: Parameters = new Parameters(MultiA, new HaltConditionIterations(10000), 30, weight = 0.5, particleWeight =  0.3, globalWeight = 0.2)
    //val pso: PSOMultObjective = new PSOMultObjective(pars, multiFunc)
    
    
    pso.runPSO()
  }
  
  /*Note that all the results are multiplied by -1. This is because these functions are specified with global
   * minimums and our PSO implementation looks for maximums
   */
  
  /**
   * Sphere test function
   */
  object Sphere extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      -solution.par.foldLeft(0.0)(_ + Math.pow(_, 2)) / solution.length
    }
  }
  
  /**
   * Rastrigin test function
   */
  object Rastrigin extends FitnessFunction {
    val A: Double = 10
    override def getFitness(solution: ParArray[Double]): Double = {
      val constant = A * solution.length
      val results = solution.par.map({
        case x => x * x - A * math.cos(2 * math.Pi * x)})
      -(constant + results.par.reduceLeft(_+_))
    }
  }
  
  /**
   * Rosenbrock test function
   */
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
  
  /**
   * AxisHyperEllipsoid test function
   */
  object AxisHyperEllipsoid extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      val range = ParArray.range(1, solution.length + 1)
      val pairs = range.zip(solution)
      val results = pairs.par.map({
        case (i,x) => i * x})
      -results.par.reduceLeft(_+_)
    }
  }
  
  /**
   * Schwefel test function
   */
  object Schwefel extends FitnessFunction {
    override def getFitness(solution: ParArray[Double]): Double = {
      val results = solution.par.map({
        case x => -x * math.sin(math.sqrt(math.abs(x)))})
      -results.par.reduceLeft(_+_)
    }
  }
  
  /**
   * Griewangk test function
   */
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
  
  /**
   * IncreasingPowerSum test function
   */
  object IncreasingPowerSum extends FitnessFunction {
    
    override def getFitness(solution: ParArray[Double]): Double = {
      val range = ParArray.range(1, solution.length + 1)
      val pairs = range.zip(solution)
      val results = pairs.par.map({
        case (i, x) => math.pow(math.abs(x), i + 1)})
      -results.par.reduceLeft(_+_)
    }
  }
  
  /**
   * Ackley test function
   */
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
  
  /**
   * MultiA test function
   */
  object MultiA extends FitnessFunction {
    
    def getFitness(solution: ParArray[Double]): Double = {
      solution.head
    }
  }
  
  /**
   * MultiB test function
   */
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