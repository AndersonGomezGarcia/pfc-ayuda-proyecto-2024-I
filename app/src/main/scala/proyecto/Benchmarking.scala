package proyecto
import datos._
import org.scalameter._

import org.scalameter.{Key, Warmer, withWarmer}

class Benchmarking {
  val objSecuencial = new Itinerario()
  val objParalelo = new ItinerarioPar()
  def itinerariosBenchmark(): Unit = {

    val timeParalela1 = config(

      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objParalelo.itinerariosPar(vuelosC1, aeropuertos)("SFO", "BOS")
      objParalelo.itinerariosPar(vuelosC1, aeropuertos)("BOS","SFO")
    }

    val timeSecuencial1 = config(

      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objSecuencial.itinerarios(vuelosC1, aeropuertos)("SFO", "BOS")
      objSecuencial.itinerarios(vuelosC1, aeropuertos)("BOS","SFO")
    }
    print("Itinerarios base:\n")
    println(s"Tiempo de ejecución Seq: $timeSecuencial1")

    println(s"Tiempo de ejecución Par: $timeParalela1")

  }
  def itinerariosTiempoBenchmark(): Unit = {

    val timeParalela2 = config(


      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 10,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objParalelo.itinerariosTiempoPar(vuelosC1, aeropuertos)("SFO", "BOS")
      objParalelo.itinerariosTiempoPar(vuelosC1, aeropuertos)("BOS","SFO")
    }


    val timeSecuencial2 = config(


      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 10,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objSecuencial.itinerariosTiempo(vuelosC1, aeropuertos)("SFO", "BOS")
      objSecuencial.itinerariosTiempo(vuelosC1, aeropuertos)("BOS","SFO")
    }
    print("Itinerarios Tiempo\n")
    println(s"Tiempo de ejecución Seq: $timeSecuencial2")

    println(s"Tiempo de ejecución Par: $timeParalela2")


  }
  def itinerariosEscalasBenchmark(): Unit = {
    val timeParalela3 = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 10,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objParalelo.itinerariosEscalasPar(vuelosC1, aeropuertos)("SFO", "BOS")
      objParalelo.itinerariosEscalasPar(vuelosC1, aeropuertos)("BOS","SFO")
    }

    val timeSecuencial3 = config(

      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 10,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objSecuencial.itinerariosEscalas(vuelosC1, aeropuertos)("SFO", "BOS")
      objSecuencial.itinerariosEscalas(vuelosC1, aeropuertos)("BOS","SFO")
    }
    print("Itinerarios Escala:\n")
    println(s"Tiempo de ejecución Seq: $timeSecuencial3")

    println(s"Tiempo de ejecución Par: $timeParalela3")

  }
  def itinerariosAireBenchmark(): Unit = {

    val timeParalela4 = config(


      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objParalelo.itinerariosAirePar(vuelosC1, aeropuertos)("SFO", "BOS")
      objParalelo.itinerariosAirePar(vuelosC1, aeropuertos)("BOS","SFO")
    }

    val timeSecuencial4 = config(

      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objSecuencial.itinerariosAire(vuelosC1, aeropuertos)("SFO", "BOS")
      objSecuencial.itinerariosAire(vuelosC1, aeropuertos)("BOS","SFO")
    }
    print("Itinerarios Aire:\n")
    println(s"Tiempo de ejecución Seq: $timeSecuencial4")

    println(s"Tiempo de ejecución Par: $timeParalela4")

  }

  def itinerariosSalidaBenchmark(): Unit = {
    val timeParalela5 = config(

      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {

      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("SFO", "BOS",5,12)
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
    }
    val timeSecuencial5 = config(

      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosSalida(vuelosCurso , aeropuertosCurso)
      /*
      objSecuencial.itinerariosSalida(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objSecuencial.itinerariosSalida(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("SFO", "BOS",5,12)
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
    }
    print("Itinerarios Salida:\n")
    println(s"Tiempo de ejecución Seq: $timeSecuencial5")
    println(s"Tiempo de ejecución Par: $timeParalela5")
  }
}