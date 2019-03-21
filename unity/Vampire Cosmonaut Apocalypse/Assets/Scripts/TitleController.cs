using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class TitleController : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
    void Update () {
        if (Input.GetKey(KeyCode.Return))
        {
            StartGame();
        }
        if (Input.GetKey(KeyCode.Escape))
        {
            Application.Quit();
        }
		
    }

    private void StartGame()
    {
        SceneManager.LoadScene("Game", LoadSceneMode.Single);
    }
}
