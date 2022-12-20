/* This library helps for holding variables. There is a MAP data structure which holds variable name (char id[32]) and value (int value) */


#ifndef IDENTIFIERS
#define IDENTIFIERS

#include <stdlib.h>
#include <string.h>

void yyerror (const char *s);
int yylex();


typedef struct
{
	char id[16];
	int value;
}	Identifier;

typedef struct
{
	Identifier* identifier;
	int size;
	int capacity;
}Map;

Map* map;

void create()
{
	map = (Map*)malloc(sizeof(Map));
	map->size = 0;
	map->capacity = 1;
	map->identifier = (Identifier*)malloc(sizeof(Identifier));
}

void update()
{
	map->capacity *= 2;
	map->identifier = (Identifier*)realloc(map->identifier, sizeof(Identifier) * map->capacity);
}

int contains(char* id)
{
	for(int i=0 ; i < map->size ; ++i)
		if(strcmp(map->identifier[i].id, id) == 0)
			return i;
	return -1;
}

void put(char* id, int val){
	int i;

	if((i = contains(id)) != -1)
	
	{
		map->identifier[i].value = val;	
        return;
	}

	if(map->size == map->capacity)
		update();

	strcpy(map->identifier[map->size].id, id);
	map->identifier[map->size].value = val;	
    ++(map->size);
}

Identifier* get(char* id)
{
	int i;

	if((i = contains(id)) == -1)
		return NULL;
	return &map->identifier[i];
}

void clear()
{
	if(map != NULL)
	{
		if(map->identifier != NULL)
			free(map->identifier);
		free(map);
	}
}

#endif