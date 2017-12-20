#include <stdio.h>
#define __USE_XOPEN
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>

#define NAME_LENGTH 50
#define EMAIL_LENGTH 50
#define PHONE_LENGTH 11
#define NAME_FORMAT "%50s"
#define EMAIL_FORMAT "[a-zA-Z0-9]@[a-zA-Z].[a-zA-Z]"
#define TIME_FORMAT "%F %T"
#define SAVE_FILE "data.txt"

typedef enum { PARLAMENT = 0, HOSOK_TERE = 1, VAR = 2 } route_t;

typedef struct {
    char name[NAME_LENGTH];
    char email[EMAIL_LENGTH];
    char phone[PHONE_LENGTH];
    route_t route;
    int num_guests;
    time_t time;
} registration_t;

typedef struct {
    int length;
    registration_t* regs;
} data_model_t;

typedef struct {
    int reg_num;
    int cost;
    route_t route;
} child_input_t;

typedef struct {
    int beer;
    int money;
} child_output_t;

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

void handler(int asd) {}

int main() {
    signal(SIGUSR1,handler);
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
        printf("  6: Masodik beadando!!!!!\n");
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
            case 6:
                {
                    int participants[3] = {0,0,0};
                    int registrations[3] = {0,0,0};
                    int i;
                    for(i = 0; i < model.length; ++i) {
                        registration_t* reg = &(model.regs[i]);
                        participants[reg->route] += 1 + reg->num_guests;
                        ++registrations[reg->route];
                    }
                    route_t route;
                    for(route = 0; route < 3; ++route) {
                        if(participants[route] >= 10) {
                            int pipefd[2];
                            pid_t pid;
                            
                            if (pipe(pipefd) == -1) {
                                perror("Hiba a pipe nyitaskor!");
                                exit(EXIT_FAILURE);
                            }
                            pid = fork();
                            if (pid == -1) {
                                perror("Fork hiba");
                                exit(EXIT_FAILURE);
                            }
                            
                            if (pid == 0) {// child process
                                kill(getppid(),SIGUSR1);
                                child_input_t input;
                                read(pipefd[0],&input,sizeof(child_input_t)); //reading
                                printf("Utvonal: %d\n", input.route);
                                child_output_t output = {0,0};
                                for(i = 0; i < input.reg_num; ++i) {
                                    printf("Regisztraciok az utvonalon: %d\n", input.reg_num);
                                    registration_t reg;
                                    printf("%s\n", reg.name);
                                    read(pipefd[0], &reg, sizeof(registration_t));
                                    output.beer += (reg.num_guests + 1) * 5;
                                    output.money += (int)(reg.num_guests * 0.85 * input.cost * 5) + (input.cost * 5);
                                }
                                close(pipefd[0]); // finally we close the used read end

                                write(pipefd[1], &output, sizeof(child_output_t));
                                close(pipefd[1]);
                                sleep(3);
                                kill(getppid(),SIGUSR1);
                                exit(0);
                            } else {// szulo process
                                child_input_t input = {registrations[route],250,route};
                                write(pipefd[1], &input, sizeof(child_input_t));
                                for(i = 0; i < model.length; ++i) {
                                    registration_t* reg = &(model.regs[i]);
                                    if(reg->route == route) {
                                        write(pipefd[1], reg, sizeof(registration_t));
                                    }
                                }
                                close(pipefd[1]); // Closing write descriptor
                                pause();
                                child_output_t lkasbdflj;
                                read(pipefd[0], &lkasbdflj, sizeof(child_output_t));
                                close(pipefd[0]);
                                pause();
                            }
                        }
                    }
                }
        }
        free(model.regs);
    }
    return 0;
}
