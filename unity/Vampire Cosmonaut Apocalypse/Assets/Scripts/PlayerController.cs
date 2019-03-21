using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour {
    public float movementSpeed = 0.05f;
    public float upperBound = 1.0f;
    public float lowerBound = -1.0f;
    public float fireRate = 0.5f;
    public GameObject missle;
    private AudioSource audioSource;
    private float nextFire = 0.0f;
    private Vector2 startingPosition;
    private Animator animator;

	// Use this for initialization
	void Start () {
        startingPosition = transform.position;
        audioSource = GetComponent<AudioSource>();
        animator = GetComponent<Animator>();
	}
	
	// Update is called once per frame
	void Update () {
        if (Input.GetKey(KeyCode.W))
        {
            if (transform.position.y < upperBound)
            {
                Vector2 offset = new Vector2(0f, movementSpeed);
                transform.position = (Vector2)transform.position + offset;
            }
        }
        if (Input.GetKey(KeyCode.S) || Input.GetKey(KeyCode.DownArrow))
        {
            if (transform.position.y > lowerBound)
            {
                Vector2 offset = new Vector2(0f, movementSpeed);
                transform.position = (Vector2)transform.position - offset;
            }
        }
        if (Input.GetKey(KeyCode.Space) || Input.GetKey(KeyCode.Mouse0))
        {
            if(Time.time > nextFire) {
                Vector2 position = new Vector2(transform.position.x + .3f, transform.position.y);
                nextFire = Time.time + fireRate;
                GameObject newInstance = Instantiate(missle, position, transform.rotation) as GameObject;
                newInstance.SetActive(true);
                audioSource.Play();
            }
        }
	}

    public void ExplodePlayer() {
        animator.SetBool("explode", true);
    }

    public void ResetPosition() {
        animator.SetBool("explode", false);
        transform.position = startingPosition;
    }
}
