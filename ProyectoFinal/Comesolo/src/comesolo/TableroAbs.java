/*
 * Esta clase va a representar una abstracción de un tablero.
 */
package comesolo;

import java.util.LinkedList;

/**
 *
 * @author emmanuel
 */
public class TableroAbs {
    LinkedList<Integer> tablero; //Representación del tablero. 0 si no tiene bola, 1 si tiene.
    
    public TableroAbs(String tab){
        tablero = new LinkedList();
        String temp = tab.substring(1,tab.length()-1); //Sacamos una subcadena que no incluya el primero ni el último.
        String[] casillas = temp.split(","); //Separamos la cadena donde están las comas.
        LinkedList<Integer> numeros = new LinkedList();
        
        for(int i=0;i<casillas.length;i++){
            numeros.add(Integer.parseInt(casillas[i]));
        }
        
        for(int i=1;i<=15;i++){ //El tamaño del tablero es 15.
            if(numeros.contains(i)){
                tablero.add(1);
            }else{
                tablero.add(0);
            }
        }
    }
    
    @Override
    public String toString(){
        return tablero.toString();
    }
}
