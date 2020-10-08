#include <stdio.h>
#include <stdlib.h>
#define MAX_STRING_LENGTH 6

/**
 * https://www.hackerrank.com/challenges/post-transition/problem
 */

struct package
{
	char* id;
	int weight;
};

typedef struct package package;

struct post_office
{
	int min_weight;
	int max_weight;
	package* packages;
	int packages_count;
};

typedef struct post_office post_office;

struct town
{
	char* name;
	post_office* offices;
	int offices_count;
};

typedef struct town town;



void print_all_packages(town t) {
    //printf("print_all_packages\n");
    printf("%s:\n", t.name);
    for(int j = 0; j < t.offices_count; ++j) {
        printf("\t%d:\n", j);
        for(int i = 0; i < t.offices[j].packages_count; ++i)
            printf("\t\t%s\n", t.offices[j].packages[i].id);
    }
}

void send_all_acceptable_packages(town* source, int source_office_index, town* target, int target_office_index) {
    //printf("send_all_acceptable_packages\n");
    //char* src_name = source->name;
    //printf("send_all_acceptable_packages %d\n", source);
    //printf("send_all_acceptable_packages %s/%d -> %s/%d\n", source->name, source_office_index, target->name, target_office_index);

    post_office* src_office = source->offices + source_office_index;
    post_office* tgt_office = target->offices + target_office_index;

    int tgt_min = tgt_office->min_weight;
    int tgt_max = tgt_office->max_weight;
    //printf("send_all_acceptable_packages %d to %d\n", tgt_min, tgt_max);

    int n_to_move = 0;
    for(int i = 0; i < src_office->packages_count; ++i) {
        package* pkg = src_office->packages + i;
        if (tgt_min <= pkg->weight && pkg->weight <= tgt_max)
            n_to_move++;
    }

    //printf("n_to_move = %d\n", n_to_move);
    if (!n_to_move)
        return;

    // this should really perform a malloc/free/assignment rather than using
    // realloc, which will fail if tgt_office->packages was originally NULL
    if (tgt_office->packages_count + n_to_move)
        tgt_office->packages = realloc(tgt_office->packages, sizeof(package) * (tgt_office->packages_count + n_to_move));

    package* new_src_packages = NULL;
    if (src_office->packages_count - n_to_move)
        new_src_packages = malloc(sizeof(package) * (src_office->packages_count - n_to_move));

    int src_i = 0;
    int tgt_i = tgt_office->packages_count;

    for(int i = 0; i < src_office->packages_count; ++i) {
        package* pkg = src_office->packages + i;
        //printf("Source package %d: %s/%d\n", i, pkg->id, pkg->weight);
        if (tgt_min <= pkg->weight && pkg->weight <= tgt_max) {
            //printf("Moving source package %d to target location %d\n", i, tgt_i);
            // why doesn't this work?
            tgt_office->packages[tgt_i] = *pkg;
            //memcpy(pkg, tgt_office->packages + tgt_i, sizeof(package));
            //(tgt_office->packages + tgt_i)->id = pkg->id;
            //(tgt_office->packages + tgt_i)->weight = pkg->weight;
            //printf("  Copied to target %d: %s/%d\n", i, (tgt_office->packages + tgt_i)->id, (tgt_office->packages + tgt_i)->weight);
            ++tgt_i;
        } else {
            //printf("Leaving source package %d at source location %d\n", i, src_i);
            // why doesn't this work?
            //new_src_packages[src_i] = *pkg;
            memcpy(new_src_packages + src_i, pkg, sizeof(package));
            //printf("  Copied to new source %d: %s/%d\n", i, (new_src_packages + src_i)->id, (new_src_packages + src_i)->weight);
            ++src_i;
        }
    }

    if (src_office->packages)
        free(src_office->packages);
    src_office->packages = new_src_packages;

    tgt_office->packages_count += n_to_move;
    src_office->packages_count -= n_to_move;

    // for(int i = 0; i < src_office->packages_count; ++i) {
    //     package* pkg = src_office->packages + i;
    //     printf("New source package %d: %s/%d\n", i, pkg->id, pkg->weight);
    // }
}

town town_with_most_packages(town* towns, int towns_count) {
    //printf("town_with_most_packages\n");
    town* town_with_most = NULL;
    int most_count = 0;
    for(int i = 0; i < towns_count; ++i) {
        town* town_i = towns + i;
        int offices_count_i = town_i->offices_count;
        int packages_count_i = 0;
        for(int j = 0; j < offices_count_i; ++j) {
            packages_count_i += town_i->offices[j].packages_count;
        }

        //printf("packages_count(town=%s) = %d\n", town_i->name, packages_count_i);

        if (packages_count_i > most_count) {
            town_with_most = town_i;
            most_count = packages_count_i;
        }
    }

    //printf("town_with_most_packages: %s\n", town_with_most->name);
    return *town_with_most;
}

town* find_town(town* towns, int towns_count, char* name) {
    //printf("find_town(%s)\n", name);
    for(int i = 0; i < towns_count; ++i) {
        town* town_i = towns + i;
        char* name_i = town_i->name;
        if (strcmp(name_i, name) == 0) {
            //printf("find_town(%s) = %d\n", name, i);
            return town_i;
        }
    }

    //printf("find_town(%s) = NULL\n", name);
    return NULL;
}

int main()
{
    //printf("MAIN\n");
	int towns_count;
	scanf("%d", &towns_count);
    //printf("towns_count = %d\n", towns_count);
	town* towns = malloc(sizeof(town)*towns_count);
	for (int i = 0; i < towns_count; i++) {
		towns[i].name = malloc(sizeof(char) * MAX_STRING_LENGTH);
		scanf("%s", towns[i].name);
        //printf("towns[%d].name = %s\n", i, towns[i].name);
		scanf("%d", &towns[i].offices_count);
		towns[i].offices = malloc(sizeof(post_office)*towns[i].offices_count);
        //printf("TOWN %s has %d offices\n", towns[i].name, towns[i].offices_count);
		for (int j = 0; j < towns[i].offices_count; j++) {
			scanf("%d%d%d", &towns[i].offices[j].packages_count, &towns[i].offices[j].min_weight, &towns[i].offices[j].max_weight);
			towns[i].offices[j].packages = malloc(sizeof(package)*towns[i].offices[j].packages_count);
			for (int k = 0; k < towns[i].offices[j].packages_count; k++) {
				towns[i].offices[j].packages[k].id = malloc(sizeof(char) * MAX_STRING_LENGTH);
				scanf("%s", towns[i].offices[j].packages[k].id);
				scanf("%d", &towns[i].offices[j].packages[k].weight);
			}
		}
	}
	int queries;
	scanf("%d", &queries);
	char town_name[MAX_STRING_LENGTH];
	while (queries--) {
		int type;
		scanf("%d", &type);
		switch (type) {
		case 1:
			scanf("%s", town_name);
			town* t = find_town(towns, towns_count, town_name);
			print_all_packages(*t);
			break;
		case 2:
			scanf("%s", town_name);
			town* source = find_town(towns, towns_count, town_name);
			int source_index;
			scanf("%d", &source_index);
			scanf("%s", town_name);
			town* target = find_town(towns, towns_count, town_name);
			int target_index;
			scanf("%d", &target_index);
			send_all_acceptable_packages(source, source_index, target, target_index);
			break;
		case 3:
			printf("Town with the most number of packages is %s\n", town_with_most_packages(towns, towns_count).name);
			break;
		}
	}


    // for (int i = 0; i < towns_count; i++) {
    //     for (int j = 0; j < towns[i].offices_count; j++) {
    //         for (int k = 0; k < towns[i].offices[j].packages_count; k++) {
    //             free(towns[i].offices[j].packages[k].id);
    //         }
    //         free(towns[i].offices[j].packages);
    //     }
    //     free(towns[i].offices);
    //     free(towns[i].name);
    // }
    // free(towns);

	return 0;
}
