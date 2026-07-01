import random

def read_int(prompt, min_val=None, max_val=None):
    while True:
        value = input(prompt)
        try:
            num = int(value)
            if min_val is not None and num < min_val:
                print(f"Wartość musi być ≥ {min_val}.")
                continue
            if max_val is not None and num > max_val:
                print(f"Wartość musi być ≤ {max_val}.")
                continue
            return num
        except ValueError:
            print("Niepoprawne dane! Proszę wpisać odpowiednią liczbę naturalną.")

def trojki2(n,m,a,b):
    trojki = []
    nlist = [j for j in range(1,n+1)]
    first = random.choice(nlist)
    nlist.remove(first)
    next = random.choice(nlist)
    nlist.remove(next)
    k = round(random.uniform(a,b),3)
    trojki.append([first,next,k])
    while nlist:
        first = next
        next = random.choice(nlist)
        nlist.remove(next)
        k = round(random.uniform(a,b),3)
        trojki.append([first,next,k])
    i = 0
    while i<m-n+1:
        x = random.randint(1,n)
        y = random.randint(1,n)
        if x==y:
            continue
        k = round(random.uniform(a,b),3)
        skip = False
        for el in trojki:
            if el[:2] in [[x,y],[y,x]]:
                skip = True
        if skip:
            continue
        trojki.append([x,y,k])
        i += 1
    return trojki

def trojki1(n,m):
    trojki = []
    print("Podaj ",m," trojek:")
    i = 0
    while i<m:
        x = read_int(f"{i+1}. x = ",min_val=1,max_val=n)
        y = read_int(f"{i+1}. y = ",min_val=1,max_val=n)
        k = read_int(f"{i+1}. k = ",min_val=1)
        skip = False
        for el in trojki:
            if el[:2] in [[x,y],[y,x]]:
                skip = True
        if skip or x==y:
            print("Powtarzająca się ścieżka bądź wpisane te same miasta!")
            continue
        trojki.append([x,y,k])
        i += 1
    ocena = czy_spojny(trojki,n)
    if ocena==True:
        return trojki
    else:
        print("Brak spójnościw grafie!")
        trojki1(n,m)

def czy_spojny(trojki,n):
    graf = [[] for _ in range(n)]
    for x,y,_ in trojki:
        graf[x-1].append(y-1)
        graf[y-1].append(x-1)
    odwiedzone = [False for _ in range(n)]
    for i in range(len(graf)):
        if len(graf[i])!=0:
            odwiedzone[i]=True
    return all(odwiedzone)

class Zbiory:
    def __init__(self, n):
        self.rodzic = list(range(1,n+1))
        self.ranga = [1] * n

    def znajdz(self,i):
        if self.rodzic[i-1] != i:
            self.rodzic[i-1] = self.znajdz(self.rodzic[i-1])
        return self.rodzic[i-1]

    def zlacz(self,a,b):
        z1 = self.znajdz(a)
        z2 = self.znajdz(b)
        if z1 != z2:
            if self.ranga[z1-1] < self.ranga[z2-1]:
                self.rodzic[z1-1] = z2
            elif self.ranga[z1-1] > self.ranga[z2-1]:
                self.rodzic[z2-1] = z1
            else:
                self.rodzic[z2-1] = z1
                self.ranga[z1-1] += 1

def kruskal(n,trojki):
    print("Lista planowanych autostrad:")
    trojki.sort(key=lambda x: x[2])
    Zb=Zbiory(n)
    koszt=0
    krawedzie=0
    while krawedzie<n-1:
        for x,y,k in trojki:
            if Zb.znajdz(x)!=Zb.znajdz(y):
                print(f"{x} -- {y}  koszt: {k}")
                Zb.zlacz(x,y)
                koszt+=k
                krawedzie+=1
    return koszt

def main():
    while True:
        print("""\nWpisz:
- 1, by automatycznie wygenerować trójki;
- 2, by samemu wpisać trójki""")
        choice = int(input("\nWybór: "))
        if choice==1:
            print("\nPodaj następujące dane do przeprowadzenia optymalizacji:")
            n = int(input(" -> liczba głównych miast: "))
            m = int(input(" -> liczba istniejących dróg: "))
            if n<1 or n>100 or m<n-1 or m>300 or m>n*(n-1)/2:
                print("Podane dane nie mieszczą się w wymaganych przedziałach!")
                continue
            print("\nPodaj dodatnie krańce przedziału automatycznego generowania:")
            a = float(input(" -> lewy kraniec przedziału: "))
            b = float(input(" -> prawy kraniec przedziału: "))
            if a<0 or b<0 or a>b:
                print("Podane krańce nie spełniają wymagań!")
                continue
            krawedzie = trojki2(n,m,a,b)
            print("Wygenerowały się nastęujące trójki:\n", krawedzie)
        elif choice==2:
            print("\nPodaj następujące dane do przeprowadzenia optymalizacji:")
            n = int(input(" -> liczba głównych miast: "))
            m = int(input(" -> liczba istniejących dróg: "))
            if n<1 or n>100 or m<n-1 or m>300 or m>n*(n-1)/2:
                print("Podane dane nie mieszczą się w wymaganych przedziałach!")
                continue
            krawedzie = trojki1(n,m)
        else:
            print("Można wybrać tylko spośród 1 lub 2!")
            continue

        koszt = kruskal(n,krawedzie)
        print(f"Minimalny koszt modernizacji dróg: {koszt}")
        koniec_prog=False
        while True:
            koniec = input("\nCzy chcesz zakończyć program? (T/N): ").strip().upper()
            if koniec == "T":
                koniec_prog=True
                break
            elif koniec == "N":
                break
            else:
                print("Nieprawidłowa odpowiedź! Wpisz T lub N!")
        if koniec_prog:
            break

if __name__ == "__main__":
    main()