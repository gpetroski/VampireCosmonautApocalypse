using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class EndController : MonoBehaviour {
    public Text scoreText;
    private GameState gameState;

	// Use this for initialization
    void Start ()
    {
        gameState = GameState.GetGameState();
    }

    
    // Update is called once per frame
    void Update () {
        scoreText.text = "Final Score: " + this.gameState.getScore().ToString();
        if (Input.GetKey(KeyCode.Escape) || Input.GetKey(KeyCode.Return))
        {
            Application.Quit();
        }
	}
}
