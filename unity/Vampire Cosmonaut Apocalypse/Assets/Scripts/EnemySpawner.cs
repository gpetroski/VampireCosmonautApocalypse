using UnityEngine;
using System.Collections;

public class EnemySpawner : MonoBehaviour
{
    public GameObject enemy;
    public float enemySpawnRate = 0.5f;
    private float nextSpawn = 0.0f;

	// Use this for initialization
	void Start()
	{

	}

	// Update is called once per frame
	void Update()
	{
        if (Time.time > nextSpawn)
        {
            nextSpawn = Time.time + enemySpawnRate;

            float y = Random.Range(-0.85f, 0.62f);
            GameObject newInstance = Instantiate(enemy, new Vector3(enemy.transform.position.x, y), enemy.transform.rotation) as GameObject;
            newInstance.SetActive(true);
        }
	}
}
