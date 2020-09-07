

// Tämä tiedosto on useimmista muista kurssin kooditiedostoista poiketen tarjolla sekä
// suomen- että englanninkielisenä (eikä vain englanniksi).



// Seuraavista alkumäärittelyistä ei tarvitse tässä vaiheessa välittää mitään.
// Ne liittyvät siihen, miten nämä funktiot on sijoitettu pakkaukseen; aiheesta luvussa 5.2.
package object o1 extends o1.util.ShortcutAliases {



  // KIRJOITA TEHTÄVISSÄ PYYDETYT FUNKTIOT SEURAAVIEN IMPORT-KÄSKYJEN ALLE:
  import scala.collection.mutable.Buffer
  import scala.math._
  
  def metreiksi(jalat: Double, tuumat: Double) = {
    ((jalat * 12 + tuumat) * 2.54) / 100
  }
  
  def kuutionTilavuus(sivu: Double) = {
    sivu * sivu * sivu
  }
  
  def kuutionAla(sivu: Double) = {
    sivu * sivu * 6
  }
  
  def tuplaaPisteet(puskuri: Buffer[Int], luku: Int): Unit = {
    puskuri(luku - 1) = (puskuri(luku - 1) * 2)
    
  }
  def sakko(elems: Buffer[Int], pelaaja: Int, vahennys: Int) = {
    val vahennetaan = min(elems(pelaaja - 1) - 1, vahennys)
    elems(pelaaja - 1) = elems(pelaaja - 1) - vahennetaan
    vahennetaan      
  } 
  def nenita(kuva: Pic, position: Pos) = {
    val pallo = circle(15, Red)
    kuva.place(pallo, position)
  }














  // Alla on yhteen luvun 1.7 tehtävistä liittyvä virheellinen koodi, joka korjataan tehtävässä.
  def kahdella(melodia: String, eka: Int, toka: Int, tauonPituus: Int) = {
    val melodiaEkalla = "[" + eka + "]" + melodia
    val melodiaTokalla = "[" + toka + "]" + melodia
    val tauko = " " * tauonPituus
    val kahdestiSoitettuna = melodiaEkalla + tauko + melodiaTokalla
  }

  // Alla on valmis pohja yhteen luvun 1.8 tehtävistä.
  // Huomaa: tästä pohjasta puuttuu varsinainen ratkaisu, ja lisäksi siinä on yksi pieni mutta vakava virhe.
  def sanallinenArvosana(tehtavaarvosana: Int, tenttibonus: Int, aktiivisuusbonus: Int) {
    val kuvaukset = Buffer("hylätty", "välttävä", "tyydyttävä", "hyvä", "erittäin hyvä", "erinomainen")
    // TÄYDENNÄ RATKAISUSI TÄHÄN. VOIT POISTAA TÄMÄN KOMMENTIN.
  }





  // TÄSSÄ ON ESIMERKKIFUNKTIOITA, JOIDEN TOTEUTUSTA KATSOTAAN LUVUISSA 1.7 JA 1.8.
  // NE ON SELITETTY TARKEMMIN LUKUJEN TEKSTISSÄ.

  def keskiarvo(eka: Double, toka: Double) = (eka + toka) / 2

  def huuda(lausahdus: String) = lausahdus + "!"

  def haukiOnKala(loppukaneetti: String) = {
    println("Kun hauki on vähärasvainen, sitä voidaan säilyttää pakastettuna jopa 6 kuukautta.")
    println("Vertailun vuoksi mainittakoon, että haukea rasvaisemman lahnan vastaava")
    println("säilymisaika on vain puolet eli 3 kuukautta.")
    println(loppukaneetti)
  }

  def piiri(sade: Double) = 2 * Pi * sade  // ei nyt käytössä luvuissa

  def etaisyys(x1: Double, y1: Double, x2: Double, y2: Double) = hypot(x2 - x1, y2 - y1)

  def punapallo(koko: Int) = circle(koko, Red)

  def isoinEtaisyys(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) = {
    val eka = etaisyys(x1, y1, x2, y2)
    val toka = etaisyys(x1, y1, x3, y3)
    val kolmas = etaisyys(x2, y2, x3, y3)
    max(max(eka, toka), kolmas)
  }

  def verot(tulot: Double, tuloraja: Double, perusprosentti: Double, lisaprosentti: Double) = {
    val perusosa = min(tuloraja, tulot)
    val lisaosa = max(tulot - tuloraja, 0)
    perusosa * perusprosentti + lisaosa * lisaprosentti
  }

  def kokeilu1(luku: Int) = {
    println("Luku on: " + luku)
  }

  def kokeilu2(lukuja: Buffer[Int]) = {
    lukuja(0) = 100
  }

  def kokeilu3(luku: Int) = {
    println("Luku on: " + luku)
    luku + 1
  }

  def kokeilu4(sana: String) = {
    var luku = 1
    println(sana + ": " + luku)
    luku = luku + 1
    println(sana + ": " + luku)
    luku = luku + 1
    println(sana + ": " + luku)
    luku
  }

  def kokeilu5(aluksi: Int) = {
    var luku = aluksi
    luku = luku + 1
    luku = luku + 1
    luku = luku + 1
    luku
  }

  def testi1(teksti: String) = {
    println(teksti)
    "aina tämä"
  }

  def testi2(teksti: String) = {
    println(teksti)
    val vastaus = testi1(huuda(teksti))
    testi1(teksti)
    println("saatiin:")
    println(vastaus)
  }






  // ALLA ON FUNKTIOITA, JOITA KÄYTETÄÄN LUVUSSA 1.6. NIIDEN SISÄISTÄ TOIMINTAA EI TARVITSE ALUKSI YMMÄRTÄÄ.
  // Seuraavaa koodia ei ole kirjoitettu aloittelijaystävälliseen tyyliin.


  def poistaNegatiiviset(lukuja: Buffer[Int]): Unit = {
    lukuja --= lukuja.filter( _ < 0 )
  }


  def imdbLeffa(sija: Int) = movieData.sortBy( _._3 ).apply(sija - 1)._1

  def imdbAikavalinParas(alkuvuosi: Int, loppuvuosi: Int) =
    movieData
      .filter( leffa => leffa._2 >= alkuvuosi && leffa._2 <= loppuvuosi )
      .sortBy( _._3 )
      .apply(0)._1

  def imdbParhaatOhjaajat(leffojaVahintaan: Int) =
    movieData
      .flatMap { case (_, _, _, _, ohjaajat) => ohjaajat.toList }
      .groupBy(identity).mapValues( _.size )
      .filter( _._2 >= leffojaVahintaan )
      .toList.sortBy( -_._2 )
      .map { case (ohjaaja, leffoja) => ohjaaja + " (" + leffoja + ")" }
      .mkString(", ")

  private lazy val movieData = {
    val Subdir   = "top_movies"
    val FileName = "omdb_movies_2015.txt"
    val rawLines = o1.util.readFileLines(s"$Subdir/$FileName").getOrElse( throw new Exception(s"Could not read the file $FileName, which is expected to be under $Subdir.") )
    val linesAsTokens = rawLines.map( _.split(";") )
    linesAsTokens.map( tokens => (tokens(0), tokens(1).toInt, tokens(2).toInt, tokens(3).toDouble, tokens(4).split(",")) )
  }


  def editointietaisyys(teksti1: String, teksti2: String) = o1.util.editDistance(teksti1, teksti2, 12)


  def animoi(kuvat: Buffer[Pic], kuviaSekunnissa: Double): Unit = {
    Animation.show(frames = kuvat, frameRate = kuviaSekunnissa)
  }

  def animoiFunktiolla(picGeneratingFunction: Int => Pic, numberOfPics: Int, picsPerSecond: Double): Unit = {
    Animation.generate(picGeneratingFunction, numberOfPics, picsPerSecond)
  }


  def kaanon(biisi: String, soittimet: Iterable[Int], viive: Int) = {
    import o1.sound.midi._
    import o1.util._

    val (melodia, tempo) = biisi match {
      case r"(.*?)$melodia(?:/([\d]+)$tempoOrNull)?" => (melodia, Option(tempoOrNull))
    }
    def alkutauko(monesko: Int) = " " * (monesko * viive.max(0).min(melodia.length))
    val eriaikaiset = for ((soitin, monesko) <- soittimet.take(MaxVoices).zipWithIndex)
                        yield s"${alkutauko(monesko)}[$soitin]$melodia"
    eriaikaiset.mkString("&") + tempo.map( "/" + _ ).getOrElse("")
  }


  def sensuroi(teksti: String, rumatSanat: Traversable[String]) =
    rumatSanat.foldLeft(teksti)( (versio, rumaSana) => versio.replaceAll(rumaSana, "[P" + "I" * max(0, rumaSana.length - 2) + "P]") )



  def pelaaPylpyrapelia(pelaaja: String) = {
    o1.gui.O1SwingDefaults()
    import o1.gui.Dialog._
    display("Tervetuloa PYLPYRÄÄTTÖRIIN, " + pelaaja + "!\nAlussa on kaksitoista pylpyrää.\n" +
            "Pelaajat ottavat vuorotellen 1 tai 2 pylpyrää.\nViimeisen pylpyrän saanut voittaa.", Centered)
    Stream.iterate(12)(pelaaKierros).takeWhile(peliJatkuu).force
    display("Valitettavasti hävisit. Sori, " + pelaaja + ".\n", Centered)

    def pelaaKierros(jaljella: Int) = {
      pyydaValinta(jaljella).map( valittu => konePelaa(jaljella - valittu) ).getOrElse(0)
    }

    def pyydaValinta(jaljella: Int) = {
      requestInt("Jäljellä on " + jaljella + " pylpyrää. Montako otat?", luku => (luku == 1 || luku == 2), "Ota 1 tai 2.", Centered)
    }

    def konePelaa(jaljella: Int) = {
      val koneOtti = parasValinta(jaljella)
      display("Otan " + koneOtti + " " + (if (koneOtti == 1) "pylpyrän" else "pylpyrää") + ".", Centered)
      jaljella - koneOtti
    }

    def peliJatkuu(jaljella: Int) = jaljella > 0

    def parasValinta(jaljella: Int) = jaljella % 3
  }


  def nayta(sana: String) = {
    println("Parametriksi saatiin: " + sana + ".")
    sana.length
  }


}
