
public class GameState {
    private static GameState gameState;
    private int score;

    private GameState() {
        score = 0;
    }

    public int getScore() {
        return score;
    }

    public void setScore(int score) {
        this.score = score;
    }

    public static GameState GetGameState() {
        if (gameState == null) {
            gameState = new GameState();
        }
        return gameState;
    }

}
