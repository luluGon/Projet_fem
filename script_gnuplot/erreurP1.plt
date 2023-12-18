reset 

set terminal png

unset key
set output 'images/erreurP1.png'

# Style du tracé, ici courbe bleu
set style line 1 lc rgb '#0074D9' lt 1 lw 2

# Titre du graphique
set title "erreur en norme L2 pour P1"



# Étiquettes des axes
set xlabel "nombre de noeuds"

set ylabel "err"

 

# Tracé de la courbe 
plot 'data/erreurP1.dat' using 1:2 with lines ls 1