using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class StarsController : MonoBehaviour {
    public float scrollSpeed;
    private BoxCollider2D stars;
    private Rigidbody2D starsRigid;
    private float groundHorizontalLength;

	// Use this for initialization
    void Start () {
        stars = gameObject.GetComponent<BoxCollider2D>();
        groundHorizontalLength = stars.size.x / 3;

        //Get and store a reference to the Rigidbody2D attached to this GameObject.
        starsRigid = GetComponent<Rigidbody2D>();

        //Start the object moving.
        starsRigid.velocity = new Vector2(scrollSpeed, 0);
    }

    //Update runs once per frame
    private void Update()
    {
        //Check if the difference along the x axis between the main Camera and the position of the object this is attached to is greater than groundHorizontalLength.
        if (transform.position.x < (-groundHorizontalLength))
        {
            //If true, this means this object is no longer visible and we can safely move it forward to be re-used.
            RepositionBackground();
        }
    }

    //Moves the object this script is attached to right in order to create our looping background effect.
    private void RepositionBackground()
    {
        //This is how far to the right we will move our background object, in this case, twice its length. This will position it directly to the right of the currently visible background object.
        Vector2 groundOffSet = new Vector2(groundHorizontalLength * 2f, 0);

        //Move this object from it's position offscreen, behind the player, to the new position off-camera in front of the player.
        transform.position = (Vector2)transform.position + groundOffSet;
    }

    void OnTriggerExit2D(Collider2D other)
    {
        if (other.gameObject.active) {
            Debug.Log("Removing object: " + other.name);
            Destroy(other.gameObject);
        }
    }
}
