/*
 * Clase que representará una jugada del comesolo.
 * Es decir, mover una bola.
 */
package comesolo;

/**
 *
 * @author emmanuel
 */
public class Jugada {
    int posIni; //La posición inicial de la bola (bola a)
    int posMid; //La bola que se va a comer (bola b)
    int posFin; //La posición final de la bola (bola a)
    
    public Jugada(){}
    
    public Jugada(int pi, int pm, int pf){
        posIni = pi;
        posMid = pm;
        posFin = pf;
    }
    
    public Jugada(String jug){
        String[] casillas = jug.split(",");
        posIni = Integer.parseInt(casillas[0])-1;
        posMid = Integer.parseInt(casillas[1])-1;
        posFin = Integer.parseInt(casillas[2])-1;
    }
    
    @Override
    public String toString(){
        return "("+posIni+","+posMid+","+posFin+")";
    }
}
