package samples;

public class BlackBoard {
    public static int main(){
        int sum, x;
        sum = 0;
        x = 1;
        while (x < 11) {
            sum = sum + 1;
            x = x + 2;
        }
        return x + sum;
    }


}


/*
    public static int main(){
        int sum, x;
        sum = 0;
        x = 1;
        while (x < 11) {
            sum = x + 1;
            x = sum + 1;
        }
        return x + sum;
    }
 */