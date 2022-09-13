package samples;

import java.util.Arrays;
import java.util.HashSet;

public class BlackBoard {
    public static void main(){
        HashSet hash = new HashSet<>(Arrays.asList(
            1,
            2,  //LEFT
            3,
            4,  //RIGHT
            5
        ));
    }
}


/*
    public static int main(){
        int x;
        BlackBoard a = new BlackBoard();
        x = 1;
        return a.m(x);
    }

    public int m(int x){
        return x;
    }

    public static void main(String[] args) {
        try {
            int[] myNumbers = {1, 2, 3};
            System.out.println(myNumbers[10]);
        } finally {
            System.out.println();
        }
    }
 */