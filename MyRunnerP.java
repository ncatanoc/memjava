public class MyRunnerP extends java.lang.Thread{
    Swap my_swap_p;
    
    MyRunnerP(Swap s){this.my_swap_p=s;}
    
    public void run(){this.my_swap_p.p();}
}
