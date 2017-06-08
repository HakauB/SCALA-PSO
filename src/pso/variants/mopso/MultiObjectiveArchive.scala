package pso.variants.mopso

import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.immutable.ParVector
import scala.collection.mutable.ListBuffer

class MultiObjectiveArchive(fitnessFunctions: ManyFitnessFunctions) {
  
  var nondominatedSolutions = new ListBuffer[Particle]()
  
  def paretoDominanceCheck(solutionA: Particle, solutionB: Particle): Boolean = {
    
    var cond1: Boolean = true
    var cond2: Boolean = false
    
    fitnessFunctions.functions.par.foreach{
      x => if(x.getFitness(solutionA.position) > x.getFitness(solutionB.position)) (cond1 = false) else (cond1 = cond1); x;
    }
    
    fitnessFunctions.functions.par.foreach{
      x => if(x.getFitness(solutionA.position) > x.getFitness(solutionB.position)) (cond2 = true) else (cond2 = cond2); x;
    }
    
    return cond1 && cond2
  }
  
  def paretoAdd(newSolution: Particle): Boolean = {
    
    var earlyBreak: Boolean = false
    
    val nondomintedSolutionsList = nondominatedSolutions.toList
    
    nondomintedSolutionsList.par.foreach{
      x => if(paretoDominanceCheck(x, newSolution)) (earlyBreak = true) else (earlyBreak = earlyBreak); x;
    }
    
    if(earlyBreak){
      return false;
    }
    
    nondomintedSolutionsList.par.foreach{
      x => if(paretoDominanceCheck(newSolution, x)) (nondominatedSolutions -= x); x;
    }
    
    nondominatedSolutions += newSolution
    
    return true;
  }
  
}
  
