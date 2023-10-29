# Projet Formation Cobol

## Présentation

Suite à une formation de **"Développeur Grands Systèmes - Cobol"** de plusieurs mois, il nous a été demandé de réaliser un projet de fin de formation. Ce dépôt rassemble donc ce projet **Mainframe**. Ce travail d'équipe a été réalisé pendant 11 jours.

## L'équipe des développeurs

Notre équipe était composé de 5 personnes :
* **Abdou-Razack Geraldo**,
* **Edouard Delbar**,
* **Maxime Flavigny**,
* **Rémi Brion**,
* et de moi-même **Christophe Vasseur**

## Les sources

Vous retrouverez dans les différents dossiers le fruit de notre travail :
* **BMS** : Les écrans **CICS** (maps)
* **COB** : Les programmes **Cobol**
* **CPY** : Les copies **Cobol** et **DB2**
* **JCL** : Les **JCL** de compilation et d'exécution
* **IMG** : Quelques exemples d'écran

# Le Projet Mainframe

## Description générale de l'application

Le projet est de réaliser une application de gestion des ventes d'album musicaux et des droits d'auteur revenant à chacun des collaborateurs d'un album dans un environnement **Mainframe ISPF/CICS/VSAM/DB2**.

La saisie des informations se fera via des écrans sous **CICS**.

Pour l’instant, vous testez **« *le meilleur des mondes* »** sans cas d’erreur pour installer l’application (les cas nominaux). Les cas alternatifs seront mis en place par la suite. 

## Maquette de l'écran Menu à proposer au lancement de l'application

L’emplacement des libellés suivants sont : 

* Le titre **« GESTION DES VENTES D ALBUMS »** est sur la 1ere ligne, 
* Le libellé **« PF12 - FIN       ENTER – VALIDATION »** est sur la 24eme ligne
* Si la personne quitte l’application un message apparaît : **« FIN DE TRANSACTION »**
* Les messages d’erreur apparaissent sur la 22eme ligne de l’écran.

L’emplacement des autres lignes est à décider par vous-même mais doit respecter l’esprit général proposé de cet écran, qui est le **« standard de l’Entreprise »** sur tous les écrans.

![Ecran du Menu Principal](https://github.com/Christophe-Vasseur/Projet-Formation-Cobol/blob/main/IMG/MenuPrinc.jpg "Menu Principal")

Chaque option de ce menu principal propose un menu de choix :

1 – CREATION D UN(E) xxxxxx
2 - CONSULTATION D UN(E) xxxxxx
3 – SUPPRESSION D UN(E) xxxxxx
4 – MODIFICATION D UN(E) xxxxxx
(5 – Liste des xxxxxx         si projet est terminé)

Un message d’information **« EN COURS DE DEVELOPPEMENT »** est à produire si le choix est en cours de développement sur l’écran du menu principal et secondaire. 

Les informations seront stockées dans des fichiers **VSAM KSDS** et tous les accès à ces fichiers sont externalisés dans des services appelés par les programmes qui gèrent les écrans.

Rappel :

* Une indépendance entre les accès physiques et les traitements doit exister, c’est-à- dire, je peux changer les accès physiques (par exemple, **VSAM** en accès **DB2**) sans modifier les traitements, c’est-à-dire sans avoir à modifier le code des programmes de gestion des fonctionnalités.

* Des obligations normes et méthodes de développement, un programme de gestion gère un seul écran et un écran est géré par un seul programme (un écran = un programme)

## Alimentation de la base relationnelle DB2 par les fichiers VSAM KSDS

Dans un premier temps, il a été décidé d’alimenter la base **DB2** existante avec les fichiers **VSAM KSDS**, mis à jour par l’application de gestion des ventes d’album.

Vous devez proposer une chaine de programmes **BATCH** pour effectuer ce travail qui met à jour la base **DB2** à partir des fichiers **VSAM KSDS** modifiés par l’application de gestion.

Une description de la base de données est donné dans l’annexe.

## Les livrables à fournir : 

* La présentation de votre projet dans un document
* Les map **BMS** 
* Les sources **COBOL/CICS** des programmes de gestion des écrans et les serveurs d’accès physiques aux données
* Le **JCL** de la chaîne de mise à jour de la base de données
* Les programmes **COBOL/DB2 BATCH** de mise à jour de la base de données

## Les noms des programmes et des écrans

* **$$** = votre groupe **G1**, **G2** ou **G3** et **?** est un numéro séquentiel ou les lettres de l’alphabet si insuffisant

* Les programmes **CICS** s’appelleront : **API$$DB?**

* Les écrans **BMS** s’appelleront **MBL$$?**

* Les codes transaction seront les 4 dernières lettres de vos **MAPS** Les codes des programmes **DB2** seront : **PGM$$???**

## Annexe : Base de données existante à alimenter

### Description des tables

   |Table CHANSON |Table ALBUM |
   |--------------------|--------------------|
   |Code SACEM*  |Code Album*  |
   |Titre Chanson |Titre Album |
   |Code Album    |            |

   |Table PERSONNE |Table MAISON_DIST |
   |--------------------|--------------------|
   |Code Personne*    |Code SIREN*  |
   |Identité Personne |Raison Sociale |
   |Code Type    |Adresse            |
   |             |Code Personne         |

   |Table CONTRAT |Table VENTES |
   |--------------------|--------------------|
   |Code Album*    |Code Album*  |
   |Code SIREN* |Date de vente* |
   |Code Personne*  |Code SIREN* |
   |Droits (en %)     |Quantité vendue   |
   |Date de signature |Prix de vente   |
   |Fonction |   |


   Les clefs de table sont marquées par un **"*"**

### La base de données

![Base de données](https://github.com/Christophe-Vasseur/Projet-Formation-Cobol/blob/main/IMG/BDD.jpg "Base de données DB2") Principal")