package samples;

public class BlackBoard {

    public static void main() {
        int x =0 , y;
        y = 1; //left
        if (x > y) {//right
            x = y+2; // left
        }else{
            y = x+3; //right
        }
    }
}
