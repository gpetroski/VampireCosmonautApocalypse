using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;

public class GameController : MonoBehaviour {
    public Text livesText;
    public Text scoreText;
    public int numberOfLives = 3;
    public PlayerController player;
    private GameState gameState;
    private float resetDelay = 0f;
    private AssetBundle sceneAssetBundle;
    private string[] scenePaths;

    // Use this for initialization
    void Start()
    {
        gameState = GameState.GetGameState();
    }

	
	// Update is called once per frame
	void Update () {
        livesText.text = numberOfLives.ToString();
        scoreText.text = this.gameState.getScore().ToString();
        if (resetDelay > 0 && Time.time > resetDelay) {
            ResetLevel();
        }
        if (Input.GetKey(KeyCode.Escape)) {
            Application.Quit();
        }
	}

    public void AddToScore(int score) {
        this.gameState.setScore(this.gameState.getScore() + score);
    }

    public void RemoveLife() {
        this.numberOfLives--;
        if (numberOfLives == 0) {
            EndGame();
        }
        this.resetDelay = Time.time + .5f;
        this.player.ExplodePlayer();
    }

    private void EndGame() {
        SceneManager.LoadScene("End", LoadSceneMode.Single);
    }

    private void ResetLevel() {
        GameObject[] enemies = GameObject.FindGameObjectsWithTag("Enemy");
        foreach (GameObject enemy in enemies)
        {
            Destroy(enemy);
        }
        GameObject[] missles = GameObject.FindGameObjectsWithTag("Missle");
        foreach (GameObject missle in missles)
        {
            Destroy(missle);
        }
        player.ResetPosition();
        resetDelay = 0f;
    }
}
