using UnityEngine;
using System.Collections;

public class Mover : MonoBehaviour
{
    public float speed = 0.1f;

    private Rigidbody2D rigidBody;

    // Use this for initialization
    void Start()
    {
        rigidBody = GetComponent<Rigidbody2D>();
        rigidBody.velocity = transform.right * speed;
    }
}
