import java.awt.*;
import java.util.concurrent.*;
import TSim.*;
import static TSim.SensorEvent.*;

public class Train extends Thread{
  private int trainId, speed;
  private TSimInterface tsi;
  private boolean enteringStation, isCritical;

  // Semaphores for the criticals sections
  private static final Semaphore[] critSems = new Semaphore[]{
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1)
  };
  // Semaphores for the station sections
  private static final Semaphore[] statSems = new Semaphore[]{
		new Semaphore(0),
		new Semaphore(0)
	};

  public final SensorEvent[] statSens =
		new SensorEvent[] {
			new SensorEvent(trainId,14,3,INACTIVE), // Station 1-top
			new SensorEvent(trainId,14,5,INACTIVE), // Station 1-bottom
			new SensorEvent(trainId,14,11,INACTIVE), // Station 2-top
			new SensorEvent(trainId,14,13,INACTIVE), // Station 2-bottom
	};

  public final SensorEvent[] critSens =
		new SensorEvent[] {
			new SensorEvent(trainId,6,5,INACTIVE), // Crossroads
			new SensorEvent(trainId,9,5,INACTIVE),
			new SensorEvent(trainId,12,7,INACTIVE),
			new SensorEvent(trainId,11,8,INACTIVE),
			new SensorEvent(trainId,14,7,INACTIVE), // Station 1-merge
			new SensorEvent(trainId,15,8,INACTIVE),
			new SensorEvent(trainId,12,9,INACTIVE),
			new SensorEvent(trainId,13,10,INACTIVE),
			new SensorEvent(trainId,7,9,INACTIVE),  // station 2-merge
			new SensorEvent(trainId,6,10,INACTIVE),
			new SensorEvent(trainId,6,11,INACTIVE),
			new SensorEvent(trainId,4,13,INACTIVE)
	};

  public Train(int trainId, int speed){
    this.trainId = trainId;
    this.speed = speed;
    tsi = TSimInterface.getInstance();
    enteringStation = false;
    isCritical = false;
  }

  public void run() {
		try {
			tsi.setSpeed(trainId, speed);
		} catch (CommandException e) {
			e.printStackTrace();
			System.exit(1);
		}
		while (true) {
			try {
				startSimulation();
			} catch (CommandException | InterruptedException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
	}

  /* Compares two sensors
   * returns "true" if they are at the same position
   * else "false"
   */
  public boolean equals(SensorEvent e1, SensorEvent e2){
    return e1.getXpos() == e2.getXpos() && e1.getYpos() == e2.getYpos();
  }

  /* Asks for permission to pass the critical section
   * Using semaphore permits
   */
  public void request(int section) throws CommandException, InterruptedException{
    tsi.setSpeed(trainId, 0);
    critSems[section].acquire();
    tsi.setSpeed(trainId, speed);
  }

  /*
   *  releases the permit for the critical section
   */
  public void signal(int section){
    critSems[section].release();
  }

  /*
   *  Checks if the sensor is a sensor of stations
   *  if so, returns true
   *  else, false
   */
  public boolean isStation(SensorEvent e){
    for(SensorEvent sensor : statSens){
      if(equals(e, sensor)) return true;
    }
    return false;
  }

  /*
   * Handles the switches where there are two tracks based on whats occupied or not
   * parameter: 1 or 0, and are different directions
   */
  private void trackMerger(int direction) throws CommandException, InterruptedException, IllegalArgumentException{
    int switchRight = TSimInterface.SWITCH_RIGHT;
    int switchLeft = TSimInterface.SWITCH_LEFT;
    if(!(direction == 1 || direction == 0))
      throw new IllegalArgumentException();
    if(critSems[3].tryAcquire()){
      if(direction == 1)
        tsi.setSwitch(15,9,switchRight);
      else tsi.setSwitch(4,9,switchLeft);
    }else{
      if(direction == 1)
        tsi.setSwitch(15,9, switchLeft);
      else tsi.setSwitch(4,9,switchRight);
    }
  }

  /*
   * Basically the brain to the simulation. Determines whether a train can or
   * is allowed to enter a critical section
   */
  private void startSimulation()
			throws CommandException, InterruptedException {
		int switchLeft = TSimInterface.SWITCH_LEFT;
		int switchRight = TSimInterface.SWITCH_RIGHT;
		SensorEvent sensor = tsi.getSensor(trainId);
    // Checks wether a station sensor is triggered
		if (isStation(sensor)) {
			if (enteringStation) {
				enteringStation = false;
				tsi.setSpeed(trainId, 0);
				sleep(2000 + 2 * Math.abs(speed));
				speed *= -1;
				tsi.setSpeed(trainId, speed);
			}
		}
    // Checks which sensor that got triggered
		else if (sensor.getStatus() == ACTIVE) {
      // is it a critical one?
			if (!isCritical) {
				// If it enters the crossroads
				if (equals(sensor, critSens[0]) ||
						equals(sensor, critSens[1]) ||
						equals(sensor, critSens[2]) ||
						equals(sensor, critSens[3])) {
					request(0);	// and asks for premission to enter
				}
        // If its leaving the upper of station 1
				else if (equals(sensor, critSens[4])) {
					request(1);	// and asks for permission to enter the merge-section
					tsi.setSwitch(17, 7, switchRight);
					trackMerger(1);
					statSems[0].release();
				}
        // If its leaving the lower of station 1
				else if (equals(sensor, critSens[5])) {
					request(1); // and asks for permission to enter the merge-section
					tsi.setSwitch(17, 7, switchLeft);
					trackMerger(1);
				}
        // If its leaving the upper of station 2
				else if (equals(sensor, critSens[10])) {
					request(2);	// and asks for permission to enter merge-section
					tsi.setSwitch(3, 11, switchLeft);
					trackMerger(0);
					statSems[1].release();
				}
        // If its leaving the lower of station 2
				else if (equals(sensor, critSens[11])) {
					request(2);	// and ask for permission to enter merge-section
					tsi.setSwitch(3, 11, switchRight);
					trackMerger(0);
				}
        // if its entering the station 1-merge
				else if (equals(sensor, critSens[6]) ||
						equals(sensor, critSens[7])) {
					request(1);	// and asks for permission to enter station-merge
					if (equals(sensor, critSens[6])) {
						critSems[3].release();
						tsi.setSwitch(15, 9, switchRight);
					} else {
						tsi.setSwitch(15, 9, switchLeft);
					}
					if (statSems[0].tryAcquire()) {
						tsi.setSwitch(17, 7, switchRight);
					} else {
						tsi.setSwitch(17, 7, switchLeft);
					}
					enteringStation = true;
				}
        // if its entering the station 2-merge
				else if (equals(sensor, critSens[8]) ||
						equals(sensor, critSens[9])) {
					request(2);	// and asks for permission to enter station-merge
					if (equals(sensor, critSens[8])) {
						critSems[3].release();
						tsi.setSwitch(4, 9, switchLeft);
					} else {
						tsi.setSwitch(4, 9, switchRight);
					}
					if (statSems[1].tryAcquire()) {
						tsi.setSwitch(3, 11, switchLeft);
					} else {
						tsi.setSwitch(3, 11, switchRight);
					}
					enteringStation = true;
				}
				isCritical = true;
			} else {
          /* Signaling that it passed by the critical section
           * and sets that its not in a critical section(false)
           */
          if (equals(sensor, critSens[0]) || equals(sensor, critSens[1]) ||
      				equals(sensor, critSens[2]) || equals(sensor, critSens[3])) {
      			signal(0);
      		} else if (equals(sensor, critSens[4]) ||
      				equals(sensor, critSens[5]) || equals(sensor, critSens[6]) ||
      				equals(sensor, critSens[7])) {
      			signal(1);
      		} else if (equals(sensor, critSens[8]) ||
      				equals(sensor, critSens[9]) || equals(sensor, critSens[10]) ||
      				equals(sensor, critSens[11])) {
      			signal(2);
      		}else{
            signal(-1);
          }
				isCritical = false;
			}
		}
	}
}
