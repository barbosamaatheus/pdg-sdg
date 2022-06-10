package samples;

public class BlackBoard {
    public static int main(){
        int sum, x;
        sum = 0;
        x = 1;
        while (x < 11) {
            if (x == 10){
                sum = sum + 1;
                x = x + 1;
            }
        }
        return x + sum;
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