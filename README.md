# λslicer

Thema:
Programmieren eines Slicers für den 3D-Druck

<img src="resources/pictures/slice_innerouter.png" width="300">

Beschreibung:
- Einlesen eines 3D-Körpers/Dreiecks-Mesh, dessen Vektoren, Normalen, und Flächen in einer mit
  XML formatierten Textdatei beschrieben sind.
  Dabei orientieren wir uns am 3MF-Standard des 3MF-Konsortiums: https://3mf.io/
- Generieren von "Slices" aus dem 3D-Körper: 2-Dimensionale Konturen, die den Umriss/die Grenzen des
  3D-Körpers auf der XY-Ebene an einem bestimmten Z-Offset beschreiben.
  Das Z-Offset beginnt bei 0 und steigt für jeden weiteren "Slice" linear mit einer konstanten Schichthöhe an.
- Generieren von raumfüllenden Kurven/Pfaden, die die 2D "Slice"-Konturen ausfüllen.
- Ausgabe dieser 2D-Pfade als Textdatei im "G-Code" Format.

Geplante vertiefende Themen:
- Anwendungsprogrammierung: Verwenden einer XML-Bibliothek zum Parsen der 3MF-Datei
- Parallelität: Das Generieren der "Slices" soll parallelisiert werden
- Funktionale Referenzen (Linsen): Es sollen Datenstrukturen zur Beschreibung des Dreiecks-Mesh und der 2D-Pfade erzeugt werden.
  Verschiedene Berechnungen, die diese Datenstrukturen verändern, sollen Gebrauch von Linsen machen.

Co-authored-by: KatjaFrey <KatjaFrey@users.noreply.github.com>

Build command: stack build --ghc-options "-O -threaded -rtsopts"
Run command: stack exec -- hslicer-exe "./resources/example_3mfs/Box/3D/3dmodel.model" "./gcodefile.gcode"
Time command: stack exec -- hslicer-exe "./resources/example_3mfs/Polygon/3D/3dmodel.model" "./gcodefile.gcode" +RTS -N -s 2>&1 | sed -n "/Total/p" 

Time Benchmark for different cpu core nrs:
for f in 1 2 3 4 5 6 7 8; do printf $f; printf ": "; stack exec -- hslicer-exe "./resources/example_3mfs/Polygon/3D/3dmodel.model" "./gcodefile.gcode" +RTS -N$f -s 2>&1 | sed -n "/Total/p";done

Threadscope:
stack build --ghc-options "-O -threaded -rtsopts -eventlog"
stack exec -- hslicer-exe "./resources/example_3mfs/Polygon/3D/3dmodel.model" "./gcodefile.gcode" +RTS -N5 -l