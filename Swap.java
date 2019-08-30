public class Swap{
  int a, b;
  
  public Swap(){
    a=1; b=2;
  }
  
  synchronized void p(){a=b;}
  
  synchronized void q(){b=a;}
    
  //@ (this.a=1 && this.b=1) || (this.a=2 && this.b=2)
  public static void main(String args[]){
    Swap s = new Swap();
    MyRunnerP thp = new MyRunnerP(s);
    MyRunnerQ thq = new MyRunnerQ(s);
    thp.start();
    thq.start();
  }
}
