import java.awt.*;
import java.util.concurrent.*;
import TSim.*;
import static TSim.SensorEvent.*;

public class Train extends Thread{
  private int trainId, speed;
  private TSimInterface tsi;
  private boolean enteringStation;
  private boolean enteringCritical;

  private static final Semaphore[] critSems = new Semaphore[]{
		new Semaphore(1),	// Cross-section semaphore
		new Semaphore(1),	// Station 1 merge section semaphore
		new Semaphore(1),	// Station 2 merge section semaphore
		new Semaphore(1)
	};

  /*
   * 4 sensorer, 2 för varje station
   */
  public final SensorEvent[] stations =
  		new SensorEvent[] {
  			new SensorEvent(trainId,0,0,INACTIVE),
  			new SensorEvent(trainId,0,0,INACTIVE),
  			new SensorEvent(trainId,0,0,INACTIVE),
  			new SensorEvent(trainId,0,0,INACTIVE),
  	};

  /*
   * 12 sensorer runt om på banan
   */
  public final SensorEvent[] criticals =
			new SensorEvent[] {
				// Critical section, cross-section
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				// Critical section, station 1 merge
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				// Critical section, station 2 merge
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE),
				new SensorEvent(trainId,0,0,INACTIVE)
	};


  public Train(int trainId, int speed)
  {
    tsi = TSimInterface.getInstance();
    this.trainId = trainId;
    this.speed = speed;
    enteringStation = true;
    enteringCritical = true;
    try{
      tsi.setSpeed(trainId, speed);
    }catch (CommandException e) {
    e.printStackTrace();
    System.exit(1);
    }
  }

  private void request(int sectionNumber) throws CommandException, InterruptedException {
  	tsi.setSpeed(trainId, 0);
  	critSems[sectionNumber].acquire();
  	tsi.setSpeed(trainId, speed);
  }

  private void signal(int sectionNumber) {
		critSems[sectionNumber].release();
	}

  private boolean isStation(SensorEvent e){
    for(SensorEvent sensor : stations){
      return sensorsEqual(e, sensor);
      }
      return false;
    }

  private boolean sensorsEqual(SensorEvent e1, SensorEvent e2){
    if(e1.getXpos() == e2.getXpos() && e1.getYpos() == e2.getYpos()){
      return true;
    }else{
      return false;
    }
  }

  public void enviromentCheck() throws CommandException, InterruptedException{
    SensorEvent sensor = tsi.getSensor(trainId);
    if(isStation(sensor)){
      if(enteringStation)
      {
        enteringStation = false;
        tsi.setSpeed(trainId, 0);
        sleep(2000 + 2 * speed * Math.abs(speed));
        speed = -speed;
        tsi.setSpeed(trainId, speed);
      }

      else if(sensor.getStatus() == ACTIVE){
        if(enteringCritical){
          // Tittar korsvägen
          if(sensorsEqual(sensor, criticals[])){
            request();
          }
          // Tittar nästa
          else if(sensorsEqual(sensor, criticals[])){
            request();
          }
          // osv...
          else if(sensorsEqual(sensor, criticals[])){
            request();
          }
        }
      }
    }
  }

  public void run(){
    while(! interrupted()){
      try {
				enviromentCheck();
			} catch (CommandException | InterruptedException e) {
				e.printStackTrace();
				System.exit(1);
			}
    }
  }
}
