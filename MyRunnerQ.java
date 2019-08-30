public class MyRunnerQ extends java.lang.Thread{
    Swap my_swap_q;

    MyRunnerQ(Swap s){this.my_swap_q=s;}

    public void run(){this.my_swap_q.q();}
}
