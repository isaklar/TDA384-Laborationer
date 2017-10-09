package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ConcurrentHashMap;

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
  private static volatile boolean flag = false;

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
          visited = new ConcurrentSkipListSet<>();
          predecessor = new ConcurrentHashMap<>();
        }

        public ForkJoinSolver(ForkJoinSolver parent, int newStart)
        {
            this(parent.maze);
            start = newStart;
            this.visited = parent.visited;
            this.predecessor = parent.predecessor;
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

    private List<Integer> parallelDepthFirstSearch()
    {
      int current = start;//maze.getBoard.getCell(player.getHeight, player.getWidth).getId();
      int player = maze.newPlayer(start);
      while(!flag){ //While the goal has not been found we continue to search
        if(maze.hasGoal(current)){
          flag = true;
          maze.move(player, current);
          return pathFromTo(start, current);
        }

        else if(maze.neighbors(current).size() == 1){ //dead end
          addVisited(current);
          maze.move(player, current);                   //Add first time bös
          return null;
        }

        else if(maze.neighbors(current).size() == 2 && current != maze.start()){ //straight road ahead
          addVisited(current);
          for(int i: maze.neighbors(current)){
            if(!visited.contains(i)){                   //Add first time bös
              maze.move(player, current);
              predecessor.put(i, current);
              current = i;
            }
          }
        }
        else{ //Reached a cross road, create new thread for each road
          maze.move(player, current);
          addVisited(current);
          ArrayList<ForkJoinSolver> childs = new ArrayList<>();

          for(int i: maze.neighbors(current)){
            //maze.setAnimate(false);
            if(!visited.contains(i)){
              ForkJoinSolver child = new ForkJoinSolver(this, i);
              childs.add(child);
              child.fork();
            }
          }
          for(ForkJoinSolver f: childs){
            List<Integer> result = f.join();
            if(result != null){
              List<Integer> returnList = pathFromTo(start, current);
              returnList.addAll(result);
              return returnList;
            }
          }
        }
      }
      return null;
    }
    private void addVisited(int id){
      visited.add(id);
      System.out.println(visited.size());
    }
}
