package samples;

public class BlackBoard {
    public static int main(){
        try
        {
            throw new NullPointerException();
        }
        catch(NullPointerException e)
        {
            System.out.println();
            throw e; // rethrowing the exception
        }
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