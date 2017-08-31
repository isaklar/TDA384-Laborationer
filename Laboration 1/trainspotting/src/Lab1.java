import TSim.*;

public class Lab1 {
  private TSimInterface tsi;

  public Lab1(Integer speed1, Integer speed2) {
    tsi = TSimInterface.getInstance();

      Train t1 = new Train(1, speed1);
      Train t2 = new Train(2, speed2);
      t1.start();
      t2.start();
  }
}
