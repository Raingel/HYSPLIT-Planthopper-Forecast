DIRECTORY: ./graphics
________________________________________________

Map background files used by various plotting programs:

   arlmap       - polygons in ascii format
   arlmap.shp	- shapefile binary equivalent

------------------------------------------------
Other map background shapfiles:

   canada.shp - Canadian provincial boundaries
   mexico.shp - Mexico provincial boundaries
   county.shp - US county boundaries
   states.shp - US state boundaries
   roads.shp  - US major roads

Sample Plotting Configuration Files:

   shapefiles_arl.txt - standard HYSPLIT map
   shapefiles_nam.txt - North America State and Province
   shapefiles_rds.txt - County and roads for highly zoomed plots

Usage:

   Copy appropriate shapefiles_???.txt to ../working and rename to
   shapefiles.txt. The plotting application program should reference
   the map background file (-j option) as shapefiles.txt, not
   ./shapefiles.txt.  An extension other than "txt" is permitted.

-----------------------------------------------
Image files used when converting output to KML/KMZ format:

   icon25.png
   icon49.png
   icon63.png
   particlelegend.png
   particle.png
