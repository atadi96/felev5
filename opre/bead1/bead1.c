#include <stdio.h>
#define __USE_XOPEN
#include <time.h>
#include <string.h>
#include <stdlib.h>

#define NAME_LENGTH 50
#define EMAIL_LENGTH 50
#define PHONE_LENGTH 11
#define NAME_FORMAT "%50s"
#define EMAIL_FORMAT "[a-zA-Z0-9]@[a-zA-Z].[a-zA-Z]"
#define TIME_FORMAT "%F %T"
#define SAVE_FILE "data.txt"

enum route { PARLAMENT = 0, HOSOK_TERE = 1, VAR = 2 };

typedef enum route route_t;

struct registration {
    char name[NAME_LENGTH];
    char email[EMAIL_LENGTH];
    char phone[PHONE_LENGTH];
    route_t route;
    int num_guests;
    time_t time;
};

typedef struct registration registration_t;

struct data_model {
    int length;
    registration_t* regs;
};

typedef struct data_model data_model_t;

int load_model(data_model_t* model) {
    FILE* file = fopen(SAVE_FILE, "r");
    if(file == NULL) {
        file = fopen(SAVE_FILE, "w");
        if (file == NULL) return 0;
        fprintf(file, "0\n");
        model->length = 0;
        model->regs = NULL;
        fclose(file);
    } else {
        int length;
        fscanf(file, "%d", &length);
        model->length = length;
        model->regs = (registration_t*)(malloc(length * sizeof(registration_t)));
        int i;
        for(i = 0; i < length; ++i) {
            int route;
            struct tm tm;
            char* timebuf[20];
            registration_t* reg = &(model->regs[i]);
            fscanf(
                file,
                "%[^;];%[^;];%[^;];%d;%d;%s",
                (char*)&(reg->name),
                (char*)&(reg->email),
                (char*)&(reg->phone),
                &route,
                &(reg->num_guests),
                (char*)timebuf
            );
            reg->route = (route_t)route;
            strptime((char*)timebuf, TIME_FORMAT, &tm);
            reg->time = mktime(&tm);
        }
        fclose(file);
    }
    return 1;
}

void print_registration(FILE* stream, const registration_t* reg) {
    char buf[20];
    strftime(buf, 20, TIME_FORMAT, localtime(&(reg->time)));
    fprintf(
        stream,
        "%s;%s;%s;%d;%d;%s\n",
        reg->name,
        reg->email,
        reg->phone,
        (int)(reg->route),
        reg->num_guests,
        buf
    );
}

int save_model_delete_reg(const data_model_t* model, const registration_t* delete_user) {
    FILE* file = fopen(SAVE_FILE, "w");
    if(file == NULL) {
        return 0;
    } else {
        fprintf(file, "%d\n", model->length - 1);
        int i;
        for(i = 0; i < model->length; ++i) {
            if(&(model->regs[i]) != delete_user) {
                print_registration(file, &(model->regs[i]));
            }
        }
        fclose(file);
        return 1;
    }
}

int save_model(const data_model_t* model, const registration_t* new_reg) {
    FILE* file = fopen(SAVE_FILE, "w");
    if(file == NULL) {
        return 0;
    } else {
        fprintf(file, "%d\n", model->length + (new_reg == NULL ? 0 : 1));
        int i;
        for(i = 0; i < model->length; ++i) {
            print_registration(file, &(model->regs[i]));
        }
        if(new_reg != NULL) {
            print_registration(file, new_reg);
        }
        fclose(file);
        return 1;
    }
}

registration_t* exists_email(const data_model_t* model, const char* email) {
    int i;
    for(i = 0; i < model->length; ++i) {
        if(strcmp(model->regs[i].email, email) == 0) {
            return &(model->regs[i]);
        }
    }
    return NULL;
}

route_t read_route() {
    printf("Utvonal valasztasa: (1: Parlament, 2: Hosok tere, 3: Var)\n");
    int route;
    scanf("%d", &route);
    return (route_t)(route - 1);
}

void read_user(registration_t* reg) {
    printf("Nev (max 50 karakter): ");
    scanf(NAME_FORMAT, reg->name);
    printf("Email (max 50 karakter): ");
    scanf(NAME_FORMAT, reg->email);
    printf("Telefonszam (11 szamjegy): ");
    scanf("%11s", reg-> phone);
    printf("Vendegek szama: ");
    scanf("%d", &(reg->num_guests));
    reg->route = read_route();
    reg->time = time(NULL);
}

void clrscr() { printf("\033[H\033[J"); }
void press_enter() {
    printf("\nUss [Enter]-t a folytatashoz.\n");
    while(getchar()!='\n');
    getchar();
}


int main() {
    int quit = 0;
    while(!quit) {
        clrscr();
        printf(" -- Sor-Kocsi Budapest --\n\n");
        printf("Elerheto funkciok:\n");
        printf("  1: Regisztracio\n");
        printf("  2: Regisztracio modositasa\n");
        printf("  3: Regisztracio torlese\n");
        printf("  4: Teljes nevsor megjelenitese\n");
        printf("  5: Utvonal nevsor megjelenitese\n");
        printf("  0: Kilepes\n");
        data_model_t model;
        load_model(&model);
        int select;
        scanf("%d", &select);
        switch(select) {
            case 0:
                quit = 1;
                break;
            case 1:
                {
                    printf("-- Regisztracio\n\n");
                    registration_t reg;
                    read_user(&reg);
                    if(exists_email(&model, reg.email) != NULL) {
                        printf("HIBA -- Mar van regisztracio ilyen email-lel! Valassza a szerkesztest vagy a torlest!");
                    } else {
                        if(save_model(&model, &reg)) {
                            printf("SIKER -- Regisztracio sikeresen mentve!");
                        } else {
                            printf("HIBA -- Hiba a regisztracio mentese kozben!");
                        }
                    }
                    press_enter();
                    break;
                }
            case 2:
                {
                    printf("-- Regisztracio modositasa\n\n");
                    char email[50];
                    printf("Email: ");
                    scanf("%50s", email);
                    registration_t* user = exists_email(&model, email);
                    if(user == NULL) {
                        printf("HIBA -- Nincs regisztracio ilyen email-lel! Elobb regisztralnia kell!");
                    } else {
                        read_user(user);
                        if(save_model(&model, NULL)) {
                            printf("SIKER -- Regisztracio sikeresen modositva!");
                        } else {
                            printf("HIBA -- Hiba a regisztracio mentese kozben!");
                        }
                    }
                    press_enter();
                    break;
                }
            case 3:
                {
                    printf("-- Regisztracio torlese\n\n");
                    char email[50];
                    printf("Email: ");
                    scanf("%50s", email);
                    registration_t* user = exists_email(&model, email);
                    if(user == NULL) {
                        printf("HIBA -- Nincs regisztracio ilyen email-lel! Elobb regisztralnia kell!");
                    } else {
                        if(save_model_delete_reg(&model, user)) {
                            printf("SIKER -- Regisztracio sikeresen torolve!");
                        } else {
                            printf("HIBA -- Hiba a regisztracio torlese kozben!");
                        }
                    }
                    press_enter();
                    break;
                }
            case 4:
                {
                    printf("-- A resztvevok nevsora\n\n");
                    int i;
                    for(i = 0; i < model.length; ++i) {
                        printf("%s\n", model.regs[i].name);
                    }
                    press_enter();
                    break;
                }
            case 5:
                {
                    route_t selected_route = read_route();
                    printf("\n-- Az adott utvonalon resztvevok nevsora\n\n");
                    int i;
                    for(i = 0; i < model.length; ++i) {
                        if(model.regs[i].route == selected_route) {
                            printf("%s\n", model.regs[i].name);
                        }
                    }
                    press_enter();
                    break;
                }
        }
        free(model.regs);
    }
    return 0;
}
