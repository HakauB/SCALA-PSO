package pso.variants.mopso

import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.immutable.ParSet

import pso.core.PSOUtils

class MultiObjArchive(fitnessFunctions: MultiFitnessFunction) {
  
  var nondominatedSolutions = ParSet[Particle]()
  
  def paretoDominanceCheck(newParticle: Particle, archiveMember: Particle): Boolean = {
    
    var cond1: Boolean = true
    var cond2: Boolean = false
    
    // Check not worse in every way
    fitnessFunctions.functions.par.foreach{
      x => if(x.getFitness(newParticle.position) < x.getFitness(archiveMember.position)) (cond1 = false)
    }
    
    // Check better in some way
    fitnessFunctions.functions.par.foreach{
      x => if(x.getFitness(newParticle.position) > x.getFitness(archiveMember.position)) (cond2 = true)
    }
    
    return cond1 && cond2
  }
  
  def paretoAdd(newSolution: Particle): Boolean = {
    
    var earlyBreak: Boolean = false
    
    // If new solution is dominated
    nondominatedSolutions.par.foreach{
      x => if(paretoDominanceCheck(x, newSolution)) (earlyBreak = true)
    }

    // We break
    if(earlyBreak){
      return false;
    }
    
    // Remove newly dominated solutions
    nondominatedSolutions = nondominatedSolutions.par.filter(x => !paretoDominanceCheck(newSolution, x))
    
    // Add the new solution
    nondominatedSolutions += newSolution
    
    return true;
  }
  
  def getLeader(): Particle = {
    nondominatedSolutions.toVector(PSOUtils.randomInt(nondominatedSolutions.size))
  }
  
  def printArchive(): Unit = {
    nondominatedSolutions.par.foreach{
      x => println(fitnessFunctions.functions(0).getFitness(x.position) + ", " + fitnessFunctions.functions(1).getFitness(x.position))
    }
  }
  
}
  
