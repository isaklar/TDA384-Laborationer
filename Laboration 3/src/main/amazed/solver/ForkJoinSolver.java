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
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.pathFromTo
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */

public class ForkJoinSolver extends SequentialSolver
{
    private static AtomicBoolean foundIt = new AtomicBoolean(false);
    private List<ForkJoinSolver> childTasks;

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
        int current = start;
        int player = maze.newPlayer(current);
        checkIfVisitedAndMark(current);
        childTasks = Collections.synchronizedList(new ArrayList<>());
        List<Integer> toReturn = null;

        while(foundIt.get() == false)
        {
            if(maze.hasGoal(current))
            {
                foundIt.set(true);
                toReturn = pathFromTo(start, current);
                break;
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
                        ForkJoinSolver childTask = new ForkJoinSolver(this, nb);
                        childTasks.add(childTask);
                        childTask.fork();
                    }
                }
            }

            if(nextSet)
            {
                maze.move(player, next);
                current = next;
            }
            else
            {
                break;
            }
        }

        for(ForkJoinSolver f : childTasks)
        {
            List<Integer> result = f.join();

            if(result != null)
            {
                toReturn = pathFromTo(start, result.get(0));
                toReturn.remove(toReturn.size() - 1);
                toReturn.addAll(result);
            }
        }

        return toReturn;
    }

    /**
     * Checks if the given current int value is part of the visited set.
     * Adds the int to the set. If the int is already part of the set it will not
     * make a difference.
     *
     * @param current The current value
     * @return If the current value is already part of the visited set
     */
    private boolean checkIfVisitedAndMark(int current)
    {
        boolean ret = visited.contains(current);
        // Doesn't matter if set already contains current
        visited.add(current);

        return ret;
    }
}
