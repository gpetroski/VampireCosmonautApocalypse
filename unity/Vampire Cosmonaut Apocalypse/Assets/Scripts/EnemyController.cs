using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EnemyController : MonoBehaviour {
    public int scoreValue = 100;
    public GameController game;
    public ExplosionController explosionController;
    private float destroyDelay = 0f;
    private Animator animator;

    // Use this for initialization
    void Start () {
        animator = GetComponent<Animator>();
        destroyDelay = 0f;
    }
    
    // Update is called once per frame
    void Update () {
        if (destroyDelay > 0f && Time.time > destroyDelay) {
            Debug.Log("End explosion: " + this.name);
            Destroy(this.gameObject);
        }
    }

    void OnTriggerEnter2D(Collider2D collider)
    {
        Debug.Log("OnTriggerEnter2D: " + collider.name);

        // Don't continue to trigger while exploding
        if (destroyDelay > 0f) {
            return;
        }
        if (collider.CompareTag("Missle")) {
            Debug.Log("Removing object: " + collider.name);
            Destroy(collider.gameObject);

            destroyDelay = Time.time + .4f;
            animator.SetBool("explode", true);
            game.AddToScore(scoreValue);
            explosionController.PlayExplosion();
        }
        if (collider.CompareTag("Player"))
        {
            Debug.Log("Collided with player");
            game.RemoveLife();
            explosionController.PlayExplosion();
        }
    }
}
