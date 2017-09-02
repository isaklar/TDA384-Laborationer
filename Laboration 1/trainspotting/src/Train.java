import java.awt.*;
import java.util.concurrent.*;
import TSim.*;
import static TSim.SensorEvent.*;

public class Train extends Thread{
  private int trainId, speed;
  private TSimInterface tsi;
  private boolean enteringStation, isCritical;

  // De kritiska semaphorerna
  private static final Semaphore[] critSems = new Semaphore[]{
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1)
  };
  // De inte så kritiska semaphorerna
  private static final Semaphore[] statSems = new Semaphore[]{
		new Semaphore(0),
		new Semaphore(0)
	};

  public final SensorEvent[] statSens =
		new SensorEvent[] {
			new SensorEvent(trainId,14,3,INACTIVE), // Top-bottom-rail
			new SensorEvent(trainId,14,5,INACTIVE), // Top-top-rail
			new SensorEvent(trainId,14,11,INACTIVE), // bottom-top-rail
			new SensorEvent(trainId,14,13,INACTIVE)
	};

  public final SensorEvent[] critSens =
		new SensorEvent[] {
			new SensorEvent(trainId,6,5,INACTIVE), //korsväg
			new SensorEvent(trainId,9,5,INACTIVE),
			new SensorEvent(trainId,12,7,INACTIVE),
			new SensorEvent(trainId,11,8,INACTIVE),
			new SensorEvent(trainId,14,7,INACTIVE), // station 1-sammafogning
			new SensorEvent(trainId,15,8,INACTIVE),
			new SensorEvent(trainId,12,9,INACTIVE),
			new SensorEvent(trainId,13,10,INACTIVE),
			new SensorEvent(trainId,7,9,INACTIVE),  // station 2-sammanfogning
			new SensorEvent(trainId,6,10,INACTIVE),
			new SensorEvent(trainId,6,11,INACTIVE),
			new SensorEvent(trainId,4,13,INACTIVE)
	};

  private final Dimension[] switches = new Dimension[]{
			new Dimension(17, 7),
			new Dimension(15, 9),
			new Dimension(4, 9),
			new Dimension(3, 11)
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

  public boolean equals(SensorEvent e1, SensorEvent e2){
    return e1.getXpos() == e2.getXpos() && e1.getYpos() == e2.getYpos();
  }

  public void request(int section) throws CommandException, InterruptedException{
    tsi.setSpeed(trainId, 0);
    critSems[section].acquire();
    tsi.setSpeed(trainId, speed);
  }

  public void signal(int section){
    critSems[section].release();
  }

  public boolean isStation(SensorEvent e){
    for(SensorEvent sensor : statSens){
      if(equals(e, sensor)) return true;
    }
    return false;
  }

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

  private void startSimulation()
			throws CommandException, InterruptedException {
		int switchLeft = TSimInterface.SWITCH_LEFT;
		int switchRight = TSimInterface.SWITCH_RIGHT;
		SensorEvent sensor = tsi.getSensor(trainId);
    // Tittar om stations sensor är triggad
		if (isStation(sensor)) {
			if (enteringStation) {
				enteringStation = false;
				tsi.setSpeed(trainId, 0);
				sleep(2000 + 2 * Math.abs(speed));
				speed = -speed;
				tsi.setSpeed(trainId, speed);
			}
		}
    // Tittar vilken av sensorerna som triggat
		else if (sensor.getStatus() == ACTIVE) {
      // Tittar om det är en kritisk
			if (!isCritical) {
				// Åker in i korsvägen
				if (equals(sensor, critSens[0]) ||
						equals(sensor, critSens[1]) ||
						equals(sensor, critSens[2]) ||
						equals(sensor, critSens[3])) {
					request(0);	//Frågar om den får åka in på korsvägen
				}
        // Lämnar övre stationen(station 1-top) och åker in på övre sammanfogningen
				else if (equals(sensor, critSens[4])) {
					request(1);	// frågar om den får åka in i övre sammanfogningen
					tsi.setSwitch(17, 7, switchRight);
					trackMerger(1);
					statSems[0].release();
				}
        // Lämnar nedre stationen(station 1-bottom) och åker in på nedre sammanfogningen
				else if (equals(sensor, critSens[5])) {
					request(1); // Frågar im den får åka in på nedre stationssammanfogningen
					tsi.setSwitch(17, 7, switchLeft);
					trackMerger(1);
				}
        // lämnar övre station 2 och åker in i station 2 sammanfogningen
				else if (equals(sensor, critSens[10])) {
					request(2);	// Begär genomgång för station 2-sammanfogning
					tsi.setSwitch(3, 11, switchLeft);
					trackMerger(0);
					statSems[1].release();
				}
        //lämnar nedre station 2 och åker in i station 2-sammanfogning
				else if (equals(sensor, critSens[11])) {
					request(2);	// Begär genomgång för station 2-sammanfogning
					tsi.setSwitch(3, 11, switchRight);
					trackMerger(0);
				}
        // Lämnar station 1s sammanfogningen och åker mot station
				else if (equals(sensor, critSens[6]) ||
						equals(sensor, critSens[7])) {
					request(1);	// Begär genomgång för station 1-sammafogning
					if (equals(sensor, critSens[6])) {
						critSems[3].release();
						tsi.setSwitch(15, 9, switchRight);
					} else {
						tsi.setSwitch(15, 9, switchLeft);
					}
					if (statSems[0].tryAcquire()) {
						tsi.setSwitch(17, 7, switchLeft);
					} else {
						tsi.setSwitch(17, 7, switchRight);
					}
					enteringStation = true;
				}
        // Åker in i station 2-sammanfogningssektionen och åker mot stationen
				else if (equals(sensor, critSens[8]) ||
						equals(sensor, critSens[9])) {
					request(2);	// Begär genomgång för station 1-sammafogning
					if (equals(sensor, critSens[8])) {
						critSems[3].release();
						tsi.setSwitch(4, 9, switchLeft);
					} else {
						tsi.setSwitch(4, 9, switchRight);
					}
					if (statSems[1].tryAcquire()) {
						tsi.setSwitch(3, 11, switchRight);
					} else {
						tsi.setSwitch(3, 11, switchLeft);
					}
					enteringStation = true;
				}
				isCritical = true;
			} else {
          //Signalerar den kritiska sektion
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
