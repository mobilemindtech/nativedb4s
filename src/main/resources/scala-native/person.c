#include "stdio.h"


typedef struct {
    char* name;
    int age;
}  Person;

void print_person(Person *p) {
    printf("name = %s, age = %i\n", (*p).name, (*p).age);
}