;; this is an alist of (<correct url> . <blob>). we want to be able to
;; find the correct url by looking at the blob alone. we could parse
;; XML and go crazy with m3u's, but let's see if a simple regex might
;; work on its own. We can't trust the XML coming from ASX servers
;; anyway.

;; the blobs are generatred automatically but the correct urls are
;; manualle inserted by taking a quick glance over the blob. generally
;; speaking, pick out the first sensible url.

'(

  ;; http://mfile.akamai.com/52334/live/reflector:36992.asx?bkup=38386
  ("mms://a993.l5233436992.c52334.g.lm.akamaistream.net/D/993/52334/v0001/reflector:36992" . "<ASX VERSION=\"3.0\">\n <ENTRY>\n  <REF HREF=\"mms://a993.l5233436992.c52334.g.lm.akamaistream.net/D/993/52334/v0001/reflector:36992\" />\n </ENTRY>\n<ENTRY>\n  <REF HREF=\"mms://a387.l5233438386.c52334.g.lm.akamaistream.net/D/387/52334/v0001/reflector:38386\" />\n </ENTRY>\n</ASX>\n")
  ;; http://mfile.akamai.com/52334/live/reflector:34354.asx?bkup=34960
  ("mms://a355.l5233434354.c52334.g.lm.akamaistream.net/D/355/52334/v0001/reflector:34354" . "<ASX VERSION=\"3.0\">\n <ENTRY>\n  <REF HREF=\"mms://a355.l5233434354.c52334.g.lm.akamaistream.net/D/355/52334/v0001/reflector:34354\" />\n </ENTRY>\n<ENTRY>\n  <REF HREF=\"mms://a961.l5233434960.c52334.g.lm.akamaistream.net/D/961/52334/v0001/reflector:34960\" />\n </ENTRY>\n</ASX>\n")
  ;; http://stream.wbai.org/64kmp3stereo.pls

  ("http://stream.wbai.org:8000/wbai_128" . "[playlist]\nFile1=http://stream.wbai.org:8000/wbai_128\nTitle1=WBAI 64K Stereo MP3 Stream \nLength1=-1\nNumberOfEntries=1\nVersion=2")
  ;; http://www.kpfa.org/streams/kpfa_64k.m3u

  ("http://streams1.kpfa.org:8000/kpfa_64" . "http://streams1.kpfa.org:8000/kpfa_64\n")
  ;; http://www.whiterosesociety.org/Kincaid.m3u
  ("http://server2.whiterosesociety.org:8000/HeadOnRadioHQ" . "#EXTM3U\n#EXTINF:-1,www.WhiteRoseSociety.org presents Head On with Bob Kincaid\nhttp://server2.whiterosesociety.org:8000/HeadOnRadioHQ\n")

  ;; http://main-fm.org/stream/high.m3u
  ("http://stream.main-fm.org:8000/main-fm-128k.mp3" . "http://stream.main-fm.org:8000/main-fm-128k.mp3\r\n")
  ;; http://sc1.christiannetcast.com:9042/listen.pls
  ("http://sc1.christiannetcast.com:9042/" . "[playlist]\nNumberOfEntries=1\nFile1=http://sc1.christiannetcast.com:9042/\n")
  ;; http://thedetour.us/player/talk.pls
  ("http://arizona.thedetour.us:8080" . "[playlist] \nnumberofentries=1 \nFile1=http://arizona.thedetour.us:8080\nTitle1=Detour - free-form radio with no boundaries! thedetour.us\nLength1=-1 \nVersion=2")
  ;; http://www.thomhartmann.com/streams/mp3stream.pls
  ("http://cast.voxcdn.net:8000/4745-WYD-stream-1" . "[playlist]\nNumberOfEntries=1\n\nFile1=http://cast.voxcdn.net:8000/4745-WYD-stream-1\nLength1=-1\n\nVersion=2\n")
  ;; http://www.krfp.org/krfp.m3u
  ("http://radiofreemoscow.org:9200/listen.pls" . "http://radiofreemoscow.org:9200/listen.pls")

  ;; http://www.181.fm/tunein.pls?station=181-frontporch&style=&description=Front%20Porch%20(Bluegrass)
  ("http://relay4.181.fm:14016" . "[playlist]\nnumberofentries=8\nFile1=http://relay4.181.fm:14016\nFile2=http://relay5.181.fm:14016\nFile3=http://relay5.181.fm:8016\nFile4=http://relay4.181.fm:8016\nFile5=http://relay2.181.fm:8016\nFile6=http://relay3.181.fm:8016\nFile7=http://relay1.181.fm:8016\nFile8=http://relay.181.fm:8016\n")

  ;; http://46.28.49.165/stream/cmr.pls
  ("http://46.28.49.165/proxy/cmr?mp=/" . "[playlist]\nnumberofentries=1\nFile1=http://46.28.49.165/proxy/cmr?mp=/\nTitle1=\nLength1=-1\nversion=2\n")

  ;; http://cast1.serverhostingcenter.com/tunein.php/brydietz/playlist.pls
  ("http://198.154.106.101:8332" . "[playlist]\nnumberofentries=1\nFile1=http://198.154.106.101:8332\nTitle1=Bluegrass MidAmerica Stream\nLength1=-1\nversion=2\n")

  ;; http://shoutcast.zfast.co.uk:2199/tunein/tfm0.pls
  ("http://178.32.5.21:8044/" . "[playlist]\nnumberofentries=1\nFile1=http://178.32.5.21:8044/\nTitle1=3TFM Stream\nLength1=-1\nversion=2\n")

  ;; this one is tricky! it starts with meta-urls that don't count!
  ;; http://www.bbc.co.uk/radio/listen/live/rs.asx
  ("mms://wmlive-nonacl.bbc.net.uk/wms/nations/scotland?BBC-UID=b5631b8e684b92452c3611dca15869b82c9a978e20301174d42f2997320606fb&amp;SSO2-UID=" . "<ASX version=\"3.0\">\n\t<ABSTRACT>http://www.bbc.co.uk/radioscotland/</ABSTRACT>\n\t<TITLE>BBC Radio Scotland</TITLE>\n\t<AUTHOR>BBC</AUTHOR>\n\t<COPYRIGHT>(c) British Broadcasting Corporation</COPYRIGHT>\n\t<MOREINFO HREF=\"http://www.bbc.co.uk/radioscotland\" />\n\t<PARAM NAME=\"HTMLView\" VALUE=\"http://www.bbc.co.uk/radioscotland\" />\n\n\t<Entry>\n\t\t<ref href=\"mms://wmlive-nonacl.bbc.net.uk/wms/nations/scotland?BBC-UID=b5631b8e684b92452c3611dca15869b82c9a978e20301174d42f2997320606fb&amp;SSO2-UID=\" />\n\t</Entry>\n</ASX>\n")

  ;; http://www.bbc.co.uk/worldservice/meta/tx/nb/live/eneuk.pls
  ("http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-eieuk" . "[playlist]\nnumberofentries=2\nFile1=http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-eieuk\nTitle1=BBC World Service English Radio\nLength1=-1\nFile2=http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-eieuk_backup\nTitle2=BBC World Service English Radio\nLength2=-1")

  ;; http://www.simplexstream.com/tunein.php/jfaulkne/playlist.pls
  ("http://5.152.208.98:8433/" . "[playlist]\nnumberofentries=1\nFile1=http://5.152.208.98:8433/\nTitle1=Cross Counties Radio\nLength1=-1\nversion=2\n")

  ;; http://www.prl24.net/prl96.asx
  ("http://radio.prl24.net:8096" . "<asx version = \"3.0\">\r\n\r\n  <entry>\r\n\r\n\t<title>Polskie Radio Londyn (96kbps)</title>\r\n\r\n\t<author>Polskie Radio Londyn</author>\r\n\r\n\t<copyright>Polskie Radio Londyn</copyright>\r\n\r\n\t<ref href=\"http://radio.prl24.net:8096\" />\r\n\r\n  </entry>\r\n\r\n</asx>\r\n\r\n")

  )
