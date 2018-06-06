/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package comesolo;

import java.awt.Point;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.LinkedList;
import processing.core.PApplet;

/**
 *
 * @author emmanuel
 */
public class Comesolo extends PApplet{

    LinkedList<String> lineas; //Esta lista va a guardar las líneas del archivo
    LinkedList<Jugada> jugadas; //Lista que guarda las jugadas.
    LinkedList<TableroAbs> tableros; //Lista que guarda los estados del tablero.
    LinkedList<Point> cbolas; //Guarda las coordenadas de las bolas
    LinkedList<Point> hoyos; //Guarda las coordenadas de los hoyos
    int tam = 600; //Tamaño de un lado de la ventana.
    TableroAbs currentTab; //El tablero actual
    Jugada currentJug; //La jugada actual
    LinkedList<Point> desp = new LinkedList<>(); //Esta lista va a ver el desplazamiento de una bola.
    int d=0; //Esta variable va a iterar sobre la lista de desplazamiento.
    int j=0; //Esta variable itera sobre la lista de jugadas.
    
    //Configuración de la ventana.
    @Override
    public void settings(){
        size(tam,tam);
    }
    
    //Configuración inicial del programa.
    @Override
    public void setup(){
        desp.clear();
        d=0;
        j=0;
        frameRate(20); //cuadros por segundo.
        background (255,255,255); //Define el color de fondo.
        lineas = new LinkedList();
        jugadas = new LinkedList();
        tableros = new LinkedList();
        cbolas = new LinkedList();
        hoyos = new LinkedList();
        leeArchivo();
        llenaJugadas();
        llenaTableros();
        llenaBolas();
        currentTab = tableros.get(0); //Iniciamos con el tablero 0
        if(!jugadas.isEmpty()){
            currentJug = jugadas.get(0); //Iniciamos con la jugada 0
            desp = trayectoria(cbolas.get(currentJug.posIni),cbolas.get(currentJug.posFin));
        }
        
    }
    
    public void mousePressed(){
        if (encima_rect){
            setup();
        }
    }
    
    @Override
    public void draw(){
        update(mouseX, mouseY);
        fill(121,85,61);
        int lado = tam-100; //Un lado del triángulo 2
        int h1 = (int)(tam*Math.sqrt(3)/2); //Altura del triangulo 1
        int h2 = (int)(lado*Math.sqrt(3)/2); //altura del triángulo 2
        fill(121,85,61); //Color del triángulo 1
        triangle(0,h1,tam/2,0,tam,h1); //dibujamos el triángulo 1
        fill(117,92,72);
        triangle(50,h2+50,tam/2,50,tam-50,h2+50); //dibujamos el triángulo 2
        
        //Dibujamos los hoyos
        fill(0,0,0);
        for(Point p:hoyos){
            ellipse(p.x,p.y,50,50);
        }
                    //Dibujamos las bolas
        for(int i=0;i<cbolas.size();i++){
            if(currentTab.tablero.get(i)==1){
                fill(134,115,161);
                Point p = cbolas.get(i);
                ellipse(p.x,p.y,50,50);
            }
        }
        if (!desp.isEmpty()){


            if(d<=20){ //Si el desplazamiento no ha terminado.
                if(d==10 && currentJug != null ){

                    currentTab.tablero.set(currentJug.posMid,0);
                }

                if(currentJug != null){
                    desplaza(currentJug.posIni);            
                }

                d++;
            }else{
                if(j<jugadas.size()-1){

                    j++; //Paso a la siguiente jugada
                    d = 0; //Reinicio el iterador de desplazamiento
                    currentJug = jugadas.get(j);
                    currentTab = tableros.get(j);
                    desp = trayectoria(hoyos.get(currentJug.posIni),hoyos.get(currentJug.posFin));
                    reiniciaBolas();
                }
            }
        }
        
        
        fill(120,40,140);
        rect(rectX , rectY , rect_ancho , rect_alto);
        fill(255,255,255);
        text("Reiniciar",45,60);
    }
    
    public boolean encimaRectangulo(int x , int y , int ancho , int alto){
        if (mouseX >= x && mouseX <= x+ancho && 
                mouseY >= y && mouseY <= y+alto) {
              return true;
        } else {
            return false;
        }
    }
    
    public int rectX = 30;
    public int rectY = 30;
    public int rect_ancho = 80;
    public int rect_alto = 50;
    public boolean encima_rect = false;
    public void update (int x , int y){
        encima_rect =this.encimaRectangulo(rectX, rectY, rect_ancho, rect_alto);
    }
    
    //Cambia la posición de una bola
    public void desplaza(int b){
        fill(255,0,0);
        ellipse(cbolas.get(b).x,cbolas.get(b).y,50,50);
        cbolas.get(b).setLocation(desp.get(d));
                   

    }
    
    //En esta parte le damos a todas las bolas su posición original.
    public void reiniciaBolas(){
        for(int i=0;i<cbolas.size();i++){
            cbolas.get(i).setLocation(hoyos.get(i));
        }
    }
    
    //Define la trayectoria a seguir de una bola en 21 pasos
    public LinkedList<Point> trayectoria(Point p1, Point p2){
        LinkedList<Point> retVal = new LinkedList();
        int dx = p2.x-p1.x; //distancia en x
        int dy = p2.y-p1.y; //distancia en y
        for(int i=0;i<=20;i++){
            Point punto = new Point(p1.x+dx*i/20,p1.y+dy*i/20);
            retVal.add(punto);
        }
        
        return retVal;
    }
    
    /**
     * Este método llena la lista de jugadas.
     */
    public void llenaJugadas(){
        String temp = lineas.get(0); //La primera línea es la que contiene las jugadas.
        if ( !temp.equals("[]")){
            temp = temp.substring(2,temp.length()-2);
            String[] casillas = temp.split("[)][,][(]"); //Separamos con esa expresión regular.

            //En esta parte agregamos las jugadas.
            for(int i=0;i<casillas.length;i++){
                Jugada j1 = new Jugada(casillas[i]);
                jugadas.add(j1);
            }

        }
    }
    
    /**
     * Este método llena las coordenadas de las bolas.
     */
    public void llenaBolas(){
        int lado = tam-100; //Un lado del triángulo
        int lado8 = lado/8; //el lado dividido entre 8
        int h = (int)((lado)*Math.sqrt(3)/2); //Altura del triángulo
        int h4 = h/4; //La altura dividida entre 4
        Point[] arr = new Point[15];
        
        arr[0] = new Point(tam/2,50);
        
        arr[1] = new Point(50+lado8*3,50+h4);
        arr[2] = new Point(50+lado8*5,50+h4);
        
        arr[3] = new Point(50+lado8*2,50+h4*2);
        arr[4] = new Point(50+lado8*4,50+h4*2);
        arr[5] = new Point(50+lado8*6,50+h4*2);
       
        arr[6] = new Point(50+lado8,50+h4*3);
        arr[7] = new Point(50+lado8*3,50+h4*3);
        arr[8] = new Point(50+lado8*5,50+h4*3);
        arr[9] = new Point(50+lado8*7,50+h4*3);
        
        arr[10] = new Point(50,50+h4*4);
        arr[11] = new Point(50+lado8*2,50+h4*4);
        arr[12] = new Point(50+lado8*4,50+h4*4);
        arr[13] = new Point(50+lado8*6,50+h4*4);
        arr[14] = new Point(50+lado8*8,50+h4*4);
        
        for(int i=0;i<arr.length;i++){
            cbolas.addLast(arr[i]);
        }
        
        for(int i=0;i<arr.length;i++){
            hoyos.add(new Point(arr[i]));
        }
    }
    
    /**
     * Este método lee el archivo de texto.
     */
    public void leeArchivo(){
        //En esta parte leemos el archivo y lo colocamos en la lista "lineas".
        try{
            FileReader fr1 = new FileReader("soluciones.sol");
            BufferedReader br1 = new BufferedReader(fr1);
            
            String linea = br1.readLine();
            
            while(linea!=null){
                lineas.add(linea);
                linea = br1.readLine();
            }
        }catch(Exception ex){}
    }
    
    /**
     * Este método llena la lista de tableros.
     */
    public void llenaTableros(){
        for(int i=1;i<lineas.size();i++){
            TableroAbs t1 = new TableroAbs(lineas.get(i));
            tableros.add(t1);
        }
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        PApplet.main("comesolo.Comesolo");
    }
    
}
