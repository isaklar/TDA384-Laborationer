package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.*;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver
{
    ForkJoinSolver neighborTask;
    ForkJoinSolver mainTask;
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelDepthFirstSearch();
    }

   /**
     * Metodversion 1
     * Fungerar snarlikt till den Metodversion 2 förutom att den
     * hanterar "visited" bättre
     * @return resultatet i labyrint
     */
    private List<Integer> parallelDepthFirstSearch(){
      int player = maze.newPlayer(start);
      int current = start;
      if(!maze.neighbors(current).isEmpty()){
        return null;
      }else if(maze.hasGoal(current)){
        maze.move(player, current);
        return pathFromTo(start, current);
      }else{
        if(!visited.contains(current)) {
          maze.move(player, current);
          visited.add(current);
          int i;
          for(int neighbor : maze.neighbors(current)) {
            if(i > 0){
              start = neighbor;
              neighborTask = new ForkJoinSolver(maze);
            }else{
              start = current;
              mainTask = new ForkJoinSolver(maze);
              i++;
            }
          }
          neighborTask.fork();
          if(mainTask.compute() != null){
            return mainTask.compute();
          }else{
            return neighborTask.join();
          }
        }
      }
    }

    /**
      * Metodversion 2
      * Fungerar snarlikt till den Metodversion 1 förutom att den inte
      * hanterar "visited" lika bra.
      * @return resultatet i labyrint
      */
    private List<Integer> parallelDepthFirstSearch() {
      int current = start;
      int player = maze.newPlayer(start);
      // returnerar null om vi är i en återvändsgränd
      if(maze.neighbors(current).isEmpty()){
        return null;
      // returnerar vägen till hjärtat om vi har hittat det.
      }else if(maze.hasGoal(current)){
        // move player to goal
        maze.move(player, current);
        // search finished.
        return pathFromTo(start, current);
      // Inget ovan? Det betyder att det finns fler vägar att gå igenom.
      }else{
        int i = 0;
        //loopar igenom alla grannar
        for(int neighbor : maze.neighbors(current)){
          // Om i är större än ett så skapar vi bara processer för grannarna...
          if(i > 0){
            start = neighbor;
            neighborTask = new ForkJoinSolver(maze);
          }else{
            // ...men bara alla förutom en, för den första som inte är större än 0
            // kommer vara "main"-processen
            start = current;
            mainTask = new ForkJoinSolver(maze);
            i++;
          }
        }
        /*
         * TODO: Lägg till så att vi kan returnera resultaten från compute och join
         * via en list<Integer>.
         *
         * Tanken är iaf att vi delar upp alla grann-processer och kör vidare på
         * main-processen genom rekursion.
         * Ger mainTask inte null så är det den vägen som leder till hjärtat.
         * annars
         * Kommer den andra tasken till slut leda till hjärtat.
         */
        neighborTask.fork();

        if(mainTask.compute() != null){
          return mainTask.compute();
        }else{
          return neighborTask.join();
        }
      }
    }
}
