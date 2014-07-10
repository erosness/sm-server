;; -*- scheme -*-
;; these babies come from requests to the TuneIn API. They are json
;; representations of TuneIn's responses. They show us some vital
;; information:
;; - is_direct may be #f and the url may still be an audio-payload
;; - many of these don't work
;; - we don't trust anybody
;;
;; we do, however, look at filename extensions. if it ends with .mp3,
;; it's an mp3-file, no matter what TuneIn tells us.

'(
  ;; change mms:// => mmst://
  ((element . "audio")
   (url . "mms://mediaserver.glauco.it/BluSat2000")
   (reliability . 83)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #t))

  ;; connection refused
  ((element . "audio")
   (url . "http://mfile.akamai.com/52334/live/reflector:36992.asx?bkup=38386")
   (reliability . 97)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #f))

  ;; connection refused
  ((element . "audio")
   (url . "http://mfile.akamai.com/52334/live/reflector:34354.asx?bkup=34960")
   (reliability . 98)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #f))

  ;; pls
  ((element . "audio")
   (url . "http://stream.wbai.org/64kmp3stereo.pls")
   (reliability . 98)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #f))

  ;; works
  ((element . "audio")
   (url . "http://icy3.abacast.com/progvoices-progvoicesmp3-32")
   (reliability . 92)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))

  ;; works
  ((element . "audio")
   (url . "http://216.55.165.146:8000")
   (reliability . 98)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t))

  ;;
  ((element . "audio")
   (url . "http://www.kpfa.org/streams/kpfa_64k.m3u")
   (reliability . 99)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #f))

  ((element . "audio")
   (url . "http://stream.am950ktnf.com:8000/")
   (reliability . 99)
   (bitrate . 56)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.whiterosesociety.org/Kincaid.m3u")
   (reliability . 98)
   (bitrate . 48)
   (media_type . "mp3")
   (is_direct . #f))

  ((element . "audio")
   (url . "http://audio.str3am.com:5110")
   (reliability . 100)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://main-fm.org/stream/high.m3u")
   (reliability . 100)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #f))


  ((element . "audio")
   (url . "http://stream-ny.radioparadise.com:8090")
   (reliability . 97)
   (bitrate . 120)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.live365.com/play/radiophx")
   (reliability . 94)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://s5.myradiostream.com:9854")
   (reliability . 85)
   (bitrate . 144)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://sc1.christiannetcast.com:9042/listen.pls")
   (reliability . 97)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #f))


  ((element . "audio")
   (url . "http://sc1.slable.com:8054/")
   (reliability . 97)
   (bitrate . 24)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://s3.voscast.com:7756")
   (reliability . 96)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://thedetour.us/player/talk.pls")
   (reliability . 94)
   (bitrate . 63)
   (media_type . "mp3")
   (is_direct . #f))


  ((element . "audio")
   (url . "http://www.airprogressive.org:8000/stream")
   (reliability . 95)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.thomhartmann.com/streams/mp3stream.pls")
   (reliability . 59)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #f))


  ((element . "audio")
   (url . "http://50.7.70.58:8309")
   (reliability . 87)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.krfp.org/krfp.m3u")
   (reliability . 100)
   (bitrate . 80)
   (media_type . "mp3")
   (is_direct . #f))


  ((element . "audio")
   (url . "http://kslconline.linfield.edu:8000/kslc.mp3")
   (reliability . 100)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://pbradio.serverroom.us:7528")
   (reliability . 10)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "mms://178.219.75.139:8080/RoyalFM")
   (reliability . 10)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://206.190.150.90:8358")
   (reliability . 97)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://206.190.150.90:8356/")
   (reliability . 72)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://wms-15.streamsrus.com:12410")
   (reliability . 92)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t))



  ((element . "audio")
   (url . "http://50.7.77.179:8024/")
   (reliability . 99)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://hpr4.hpr.org/")
   (reliability . 49)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://95.211.99.13/listen/khe45wxkb")
   (reliability . 97)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://wamu-2.streamguys.com:80")
   (reliability . 98)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://wamu.org/streams/live/2/live.asx")
   (reliability . 60)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #f))





  ((element . "audio")
   (url . "http://wms-15.streamsrus.com:12410")
   (reliability . 92)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://listen.radionomy.com/HitsRadioCountry")
   (reliability . 100)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://listen64.radionomy.com/HitsRadioCountry.m3u")
   (reliability . 98)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #f))



  ((element . "audio")
   (url . "http://listen.radionomy.com/radio-rockabilly-country-front")
   (reliability . 92)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://stream2.wnrvbluegrassradio.com/live-mp3")
   (reliability . 98)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://www.wnrvbluegrassradio.com/wnrv-live-stream.asx")
   (reliability . 50)
   (bitrate . 64)
   (media_type . "wma")
   (is_direct . #f))


  ((element . "audio")
   (url . "http://listen.radionomy.com/AbacusfmCountry")
   (reliability . 84)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url .
        "http://www.181.fm/tunein.pls?station=181-frontporch&style=&description=Front%20Porch%20(Bluegrass)")
   (reliability . 98)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #f)
   (element . "audio")
   (url . "http://www.181.fm/stream/asx/181-frontporch")
   (reliability . 64)
   (bitrate . 64)
   (media_type . "wma")
   (is_direct . #f))



  ((element . "audio")
   (url . "http://50.7.77.179:8024/")
   (reliability . 99)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://hpr4.hpr.org/")
   (reliability . 49)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://tess.fast-serv.com:8184")
   (reliability . 97)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://radio.worldwidebluegrass.com:8700")
   (reliability . 91)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://stream2.ndpusa.com:21009")
   (reliability . 97)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://listen.radionomy.com/CountryCrossroadsRadio")
   (reliability . 97)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://s7.viastreaming.net:7430/")
   (reliability . 99)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://waaj-1.dlinkddns.com:8081")
   (reliability . 98)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://184.95.62.170:9242/")
   (reliability . 100)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://master.streamonomy.com/roothog")
   (reliability . 98)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://s4.total-streaming.com:8074/")
   (reliability . 98)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))



  ((element . "audio")
   (url . "http://wamu-2.streamguys.com:80")
   (reliability . 98)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))



  ((element . "audio")
   (url . "http://www.live365.com/play/texasman")
   (reliability . 95)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "mms://nick9.surfernetwork.com/WMTL")
   (reliability . 94)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://50.22.212.205:8022")
   (reliability . 94)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.live365.com/play/kahfluie")
   (reliability . 94)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))



  ((element . "audio")
   (url . "http://moon.wavestreamer.com:7732")
   (reliability . 81)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://listen.radionomy.com/telluridebluegrassradio")
   (reliability . 88)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))



  ((element . "audio")
   (url . "http://sc2.spacialnet.com:34064")
   (reliability . 92)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://46.28.49.165/stream/cmr.pls")
   (reliability . 86)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #f))


  ((element . "audio")
   (url .
        "http://cast1.serverhostingcenter.com/tunein.php/brydietz/playlist.pls")
   (reliability . 91)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #f))



  ((element . "audio")
   (url . "http://listen.radionomy.com/est-en-ouest-radio")
   (reliability . 97)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.live365.com/play/radiowayne")
   (reliability . 87)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))

  ((element . "audio")
   (url . "http://calm11.calmradio.com:8646/")
   (reliability . 100)
   (bitrate . 56)
   (media_type . "mp3")
   (is_direct . #t)
   (element . "audio")
   (url . "http://calmradio.com/playlists-free/bluegrass56.pls")
   (reliability . 10)
   (bitrate . 56)
   (media_type . "mp3")
   (is_direct . #f))



  ((element . "audio")
   (url . "http://85.214.123.88:19000")
   (reliability . 100)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://www.knewmusic.com:8000/")
   (reliability . 100)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://184.171.160.162:8752")
   (reliability . 45)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))

  ((element . "audio")
   (url . "mms://netshow6.play.cz/country128")
   (reliability . 10)
   (bitrate . 128)
   (media_type . "wma")
   (is_direct . #t))

  ((element . "audio")
   (url .
        "http://soundbeat.org/wp-content/uploads/2014/03/PATDOYLE-Bagpipes_mixdown-.mp3")
   (reliability . 75)
   (bitrate . 20)
   (media_type . "mp3")
   (is_direct . #t))

  ((element . "audio")
   (url . "http://radio.talksport.com/stream?awparams=platform:ts-web;lang:en")
   (reliability . 99)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #t))


  ((element . "audio")
   (url . "http://shoutcast.zfast.co.uk:2199/tunein/tfm0.pls")
   (reliability . 98)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #f))

  ((element . "audio")
   (url . "http://stream.amazingradio.co.uk:8000/")
   (reliability . 96)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://www.bbc.co.uk/radio/listen/live/rs.asx")
   (reliability . 97)
   (bitrate . 48)
   (media_type . "wma")
   (is_direct . #f))
  ((element . "audio")
   (url . "http://www.bbc.co.uk/worldservice/meta/tx/nb/live/eneuk.pls")
   (reliability . 96)
   (bitrate . 32)
   (media_type . "mp3")
   (is_direct . #f)
   (element . "audio")
   (url . "http://www.bbc.co.uk/worldservice/meta/live/nb/eieuk_au_nb.asx")
   (reliability . 82)
   (bitrate . 32)
   (media_type . "wma")
   (is_direct . #f))
  ((element . "audio")
   (url . "http://usa2-pn.mixstream.net:8466/")
   (reliability . 95)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://media-ice.musicradio.com/CapitalXTRANationalMP3")
   (reliability . 91)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://www.simplexstream.com/tunein.php/jfaulkne/playlist.pls")
   (reliability . 95)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #f))

  ((element . "audio")
   (url . "http://radio.canstream.co.uk:8070/live.mp3")
   (reliability . 97)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://audio1.ipercast.net/frenchradiolondon.com/hi/mp3")
   (reliability . 95)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #t))

  ((element . "audio")
   (url . "http://s1.voscast.com:7918")
   (reliability . 96)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://media3.ultrastream.co.uk:80/passionfortheplanet")
   (reliability . 97)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #t))

  ((element . "audio")
   (url . "http://www.prl24.net/prl96.asx")
   (reliability . 98)
   (bitrate . 96)
   (media_type . "wma")
   (is_direct . #f))

  ((element . "audio")
   (url . "http://ar.canstream.co.uk:8003/live.mp3")
   (reliability . 88)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://mp3streaming.planetwideradio.com:9360/RadioYorks")
   (reliability . 97)
   (bitrate . 96)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://67.159.60.45:8096")
   (reliability . 81)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://tx.sharp-stream.com/icecast.php?i=smilesussex.mp3")
   (reliability . 93)
   (bitrate . 128)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://media-ice.musicradio.com/SmoothUKMP3")
   (reliability . 93)
   (bitrate . 320)
   (media_type . "mp3")
   (is_direct . #t))
  ((element . "audio")
   (url . "http://tx.sharp-stream.com/icecast.php?i=spectrum2.mp3")
   (reliability . 96)
   (bitrate . 64)
   (media_type . "mp3")
   (is_direct . #t))
  )
