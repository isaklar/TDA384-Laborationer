import java.awt.*;
import java.util.concurrent.*;
import java.util.*;
import TSim.*;
import static TSim.SensorEvent.*;

public class Train extends Thread{
  private int trainId, speed;
  private int left, right;
  private TSimInterface tsi;
  private boolean enteringstation = false;
  private boolean direction;
  private static final Semaphore s1 = new Semaphore(1);
  private static final Semaphore s2 = new Semaphore(1);
  private static final Semaphore s3 = new Semaphore(1);
  private static final Semaphore s4 = new Semaphore(1);
  private static final Semaphore s5 = new Semaphore(1);
  private static final Semaphore s6 = new Semaphore(1);

  private HashMap<Integer, ArrayList<Integer>> sensors;

  public Train(int trainId, int speed) throws InterruptedException{
    tsi = TSimInterface.getInstance();
    left = TSimInterface.SWITCH_LEFT;
    right = TSimInterface.SWITCH_RIGHT;

    if(trainId == 1){ // up-down = true
      direction = true;
      s2.acquire();
    }
    else{
      direction = false;//down-up = false
      s6.acquire();
    }

    this.trainId = trainId;
    this.speed = speed;

    sensors = new HashMap<>();
  }
  public void run(){
    try {
      tsi.setSpeed(trainId,speed);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
    try{
      tsi.setSwitch(17, 7, right);
      tsi.setSwitch(15, 9, right);
      fillSensorList();
      while(! interrupted()){
      startSimulation();}
    }
    catch (InterruptedException | CommandException e){
      e.printStackTrace();
      System.exit(1);
    }

  }
  private void fillSensorList(){
      for(int i = 1; i < 17; i++){
    sensors.put(i, new ArrayList<Integer>());
    }
    sensors.get(1).addAll(Arrays.asList(14, 3)); // 1-4 is stations
    sensors.get(2).addAll(Arrays.asList(14, 5));
    sensors.get(3).addAll(Arrays.asList(14, 11));
    sensors.get(4).addAll(Arrays.asList(14, 13));
    sensors.get(5).addAll(Arrays.asList(6, 5)); // 5-16 is critical stations
    sensors.get(6).addAll(Arrays.asList(9, 5));
    sensors.get(7).addAll(Arrays.asList(11, 7));
    sensors.get(8).addAll(Arrays.asList(14, 7));
    sensors.get(9).addAll(Arrays.asList(10, 8));
    sensors.get(10).addAll(Arrays.asList(15, 8));
    sensors.get(11).addAll(Arrays.asList(12, 9));
    sensors.get(12).addAll(Arrays.asList(7, 9));
    sensors.get(13).addAll(Arrays.asList(13, 10));
    sensors.get(14).addAll(Arrays.asList(6, 10));
    sensors.get(15).addAll(Arrays.asList(6, 11));
    sensors.get(16).addAll(Arrays.asList(4, 13));
  }
  private void isStation()throws InterruptedException, CommandException{
    if(enteringstation){
      enteringstation = false;
      tsi.setSpeed(trainId, 0);
      sleep(2000 + 2 * Math.abs(speed));
      speed = -speed;
      direction ^= true;
      tsi.setSpeed(trainId, speed);
    }
  }
  private void acquireSemaphore(Semaphore s) throws CommandException, InterruptedException{
    tsi.setSpeed(trainId, 0);
    System.out.print("\nTrying to Acquire Semapohre: " + s.toString());
    s.acquire();
    tsi.setSpeed(trainId, speed);
  }
  private void startSimulation() throws CommandException, InterruptedException {

    SensorEvent sensor = tsi.getSensor(trainId);

    ArrayList<Integer> coordinates = new ArrayList<Integer>();
    coordinates.addAll(Arrays.asList(sensor.getXpos(), sensor.getYpos()));

    int currentSensor = 0;
    for(int i = 1; i <17; i++){ // Checks which of the keys in the HashMap corresponds to the coordinates of the sensor
      if(sensors.get(i).get(0) == coordinates.get(0))
        if(sensors.get(i).get(1) == coordinates.get(1)){
          currentSensor = i;
        }
    }
    if(sensor.getStatus() == 0x01){
    switch(currentSensor){
      case 1: case 2: case 3: case 4:
        isStation(); //1-4 are stations
        break;
      case 5:
        if(direction){
          acquireSemaphore(s1);
        }
        else {
          s1.release();
          enteringstation = true;
         }
        break;
      case 6:
        if(direction){
          acquireSemaphore(s1);
        }
        else {
          s1.release();
          enteringstation = true;
        }
        break;
      case 7:
        if(direction){
          s1.release();
        }
        else{
          acquireSemaphore(s1);
        }
        break;
      case 8:
        if(direction){
          acquireSemaphore(s3);
          s2.release();
          tsi.setSwitch(17, 7, right);
          if(! s4.tryAcquire()){
            tsi.setSwitch(15, 9, left);
          }
          else{
            tsi.setSwitch(15, 9, right);
          }
        }
        else{
          s3.release();
        }
        break;
      case 9:
        if(direction){
          s1.release();
        }
        else{
          acquireSemaphore(s1);
        }
        break;
      case 10:
        if(direction){
          acquireSemaphore(s3);
          tsi.setSwitch(17, 7, left);
          if(! s4.tryAcquire()){
            tsi.setSwitch(15, 9, left);
          }
          else{
            tsi.setSwitch(15, 9, right);
          }
        }
        else{
          s3.release();
        }
        break;
      case 11:
        if(direction){
          s3.release();

        }
        else{
          acquireSemaphore(s3);
          s4.release();
          tsi.setSwitch(15, 9, right);
          if(!s2.tryAcquire()){
              tsi.setSwitch(17, 7, left);
          }
          else{
              tsi.setSwitch(17, 7, right);
          }
        }
        break;
      case 12:
        if(direction){
          acquireSemaphore(s5);
          tsi.setSwitch(4, 9, left);
          if(! s6.tryAcquire()){
            tsi.setSwitch(3, 11, right);
          }
          else{
            tsi.setSwitch(3, 11, left);
          }
        }
        else{
          s5.release();
        }
        break;
      case 13:
        if(direction){
          s3.release();
        }
        else{
          acquireSemaphore(s3);
          tsi.setSwitch(15, 9, left);
        }
        break;
      case 14:
        if(direction){
          acquireSemaphore(s5);
          tsi.setSwitch(4, 9, right);
          if(! s6.tryAcquire()){
            tsi.setSwitch(3, 11, right);
          }
          else{
            tsi.setSwitch(3, 11, left);
          }
        }
        else{
          s5.release();
        }
        break;
      case 15:
        if(direction){
          enteringstation = true;
          s4.release();
          s5.release();
        }
        else{
          acquireSemaphore(s5);
          s6.release();
          tsi.setSwitch(3, 11, left);
          if(!s4.tryAcquire()){
            tsi.setSwitch(4, 9, right);
          }
          else{
            tsi.setSwitch(4, 9, left);
          }
        }
        break;
      case 16:
        if(direction){
          enteringstation = true;
          s4.release();
          s5.release();
        }
        else{
          acquireSemaphore(s5);
          tsi.setSwitch(3, 11, right);
          if(! s4.tryAcquire()){
            tsi.setSwitch(4, 9, right);
          }
          else{
            tsi.setSwitch(4, 9, left);
          }
        }
        break;

    }
    }
  }
}
