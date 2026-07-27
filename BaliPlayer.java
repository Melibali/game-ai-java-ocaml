package cc;

import java.util.concurrent.ThreadLocalRandom;

import game.Game;
import main.collections.FastArrayList;
import other.AI;
import other.context.Context;
import other.state.container.ContainerState;
import other.move.Move;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.HashMap;

public class BaliPlayer extends AI
{		
	public final static String ocaml_interpreter = "METTRE LE CHEMIN VERS L'INTERPRETEUR OCAML ICI";
	public final static String ocaml_program = "Player.ml";
	public final static String player_name = "AI_PLAYER";

	protected int player = -1; // player_index
	
	public BaliPlayer()	{
			this.friendlyName = player_name;
	}
	
	@Override
	public Move selectAction
	(
		final Game game, 
		final Context context, 
		final double maxSeconds,
		final int maxIterations,
		final int maxDepth
	)
	{
		FastArrayList<Move> legalMoves = game.moves(context).moves();
    int board[] = new int[36];
		for (final ContainerState containerState : context.state().containerStates()) {
			for(int i = 0; i < 36; i++) {
				board[i] = containerState.whoCell(i);
			}
		}
		StringBuilder sb = new StringBuilder();
		for(int i = 0; i < 36; i++) {
			sb.append(""+board[i]);			
		}
		sb.append(" "+player);
		try {
			System.err.println("[info] "+sb.toString()+" "+player);
			Process process = startProcessLocal(sb.toString(), ""+player);
			String res = endProcessLocal(process);
      String[] resParts = res.split(":");
			if(resParts.length == 2) {
				int pos_i = Integer.parseInt(resParts[0]);
				int pos_f = Integer.parseInt(resParts[1]);
				for(int i = 0; i < legalMoves.size(); i++) {
				  if(legalMoves.get(i).from() == pos_i && legalMoves.get(i).to() == pos_f) { 
						System.err.println(""); 
						return legalMoves.get(i);
					}
		  	}
			} else {
				System.err.println("[info] playing random, program "+res+" ERROR");
			}		
    } catch(Exception e) { 
      e.printStackTrace();
    }
		return legalMoves.get(0);
	}	

	@Override
	public void initAI(final Game game, final int playerID)
	{
		this.player = playerID;
	}	
	public Process startProcessLocal(String _board, String _turn) throws IOException
	{
		ProcessBuilder processBuilder = new ProcessBuilder(ocaml_interpreter,ocaml_program,_board,_turn);
		processBuilder.redirectErrorStream(false);
		try {
			return processBuilder.start();
		} catch(Exception e) { System.err.println(e); }
		return null;
	}
	public String endProcessLocal(Process process) throws IOException, InterruptedException 
  {
		StringBuilder processOutput = new StringBuilder();
		StringBuilder processErr = new StringBuilder();
		try (BufferedReader processOutputReader = new BufferedReader(
						new InputStreamReader(process.getInputStream()));) {
				String readLine;
				while ((readLine = processOutputReader.readLine()) != null) {
						processOutput.append(readLine + System.lineSeparator());
				}
				process.waitFor();
		} catch(Exception e) { System.err.println(e); } 
		try (BufferedReader processOutputReader = new BufferedReader(
						new InputStreamReader(process.getErrorStream()));) {
				String readLine;
				while ((readLine = processOutputReader.readLine()) != null) {
						processErr.append(readLine + System.lineSeparator());
				}
				process.waitFor();
		} catch(Exception e) { System.err.println(e); } 		
		return processOutput.toString().trim();
	}
}
