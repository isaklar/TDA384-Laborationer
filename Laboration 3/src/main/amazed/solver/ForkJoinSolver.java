package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
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
    private ForkJoinSolver parentTask;
    private List<ForkJoinSolver> childTasks;    
    // Might have to use something in the line of
    // https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/atomic/AtomicReference.html
    // instead of static variables. But as far sa we have understood it
    // It should stil be OK to do it that way.
    private static volatile boolean foundIt;
    private static volatile Set<Integer> visited;

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
        initVisited();
    }

    public ForkJoinSolver(Maze maze, int newStart, Map<Integer, Integer> predecessor)
    {
        this(maze);
        start = newStart;
        this.visited = visited;
        this.predecessor = predecessor;
    }

    private static synchronized void initVisited()
    {
        if(visited == null)
        {
            visited = new HashSet<>();
        }
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
        int current = start;
        int player = maze.newPlayer(current);
        visited.add(current);
        childTasks = new ArrayList<>();

        while(!foundIt)
        {
            if(maze.hasGoal(current))
            {
                foundIt = true;
                return pathFromTo(start, current);
            }

            int next = 0;
            boolean nextSet = false;

            for(int nb : maze.neighbors(current))
            {
                if(!checkIfVisitedAndMark(nb))
                {
                    predecessor.put(nb, current);
                    if(!nextSet)
                    {
                        next = nb;
                        nextSet = true;
                    }
                    else
                    {
                        ForkJoinSolver childTask =
                            new ForkJoinSolver(maze, nb, predecessor);
                        childTasks.add(childTask);
                        childTask.fork();
                    }
                }
            }
            if(!nextSet)
            {
                break;
            }

            maze.move(player, next);
            current = next;
        }

        for(ForkJoinSolver f : childTasks)
        {
            List<Integer> result = f.join();

            if(result != null)
            {
                return this.pathFromTo(start, result.get(result.size() - 1));
            }
        }

        return null;
    }

    /**
     * Synchronized check if the given curren int value is part of the visited set.
     * Adds the int to the set. If the int is already part of the set it will not
     * make a difference.
     *
     * @param current The current value
     * @return If the current value is already part of the visited set
     */
    private static synchronized boolean checkIfVisitedAndMark(int current)
    {
        boolean ret = visited.contains(current);
        // Doesn't matter if set already contains current
        visited.add(current);

        return ret;
    }
}
