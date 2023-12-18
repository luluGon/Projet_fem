reset 

set terminal png

unset key
set output 'images/erreurP2_log.png'

# Style du tracé, ici courbe bleu
set style line 1 lc rgb '#0074D9' lt 1 lw 2

# Titre du graphique
set title "erreur en norme L2 pour P2"


# Étiquettes des axes
set xlabel "-log10(nombre de noeuds) "

set ylabel "log10(err)"

 

# Tracé de la courbe 
plot 'data/erreurP2.dat' using 3:4 with lines ls 1
