// -----------------------------------------------------------------------
//  Interface between a fortran MC generator and PSTree (C++).
//  
//  Author: David Hall
//  Date:   26th April 2012
//  
//  This file defines functions accessible to the EvGen and declares a
//  PSTree class object, which will handle the analysis and output
//  of the events.
//  
//  There are four functions which are called at different points in the
//  EvGen routine:
//  INITROOT    - called at start of event generation
//  ADDPARTICLE - called for every final state particle
//  FILLROOT    - called for every event
//  CLOSEROOT   - called at end of event generation
//  
//  It is simple to pass new information to the PSTree object.
//  A variable must be added to a common block in the EvGen analysis
//  file (e.g. mcatnlo_hwanPSTree.f) and then added to the respective
//  extern struct in this file. See below for more information.
//  You would then need to update the PSTree class to transfer this 
//  new information using the AddParticle or AddEvent methods.
// -----------------------------------------------------------------------

/*
// Standard libraries
#include <iostream>
#include <fstream>
#include <cstring>

// PSTree
#include "PSTree.h"


#ifdef __cplusplus
extern "C" {
#endif
  //------------------------------------------------------------------------
  // C++ functions accessible by Fortran.
  // In Fortran, remove the trailing underscore.
  // Preprocessor statements prevent name mangling.
  //------------------------------------------------------------------------
  void initroot_(const char *configFilename, int lconfigFilename);
  void addparticle_();
  void fillroot_();
  void writeroot_();
  void closeroot_();
  void rclos_();

//------------------------------------------------------------------------
// Fortran common block variables accessible by C++.
// Variable ordering must be consistent between the two languages.
// Follow the sequence: double, float, int, bool to ensure they
// are located contiguously in memory.
//------------------------------------------------------------------------
  extern struct{
    double wgt,q2ren,q2fact,x1,x2;
    int fl1,fl2;
  } event_;

  extern struct{
    double px,py,pz,e;
    int id;
  } particle_;
#ifdef __cplusplus
}//extern "C"
#endif

*/

#include "fInterface.h"

// Declare global pointers
PSTree *eventHandler;

//char* strFtoC(const char *str, int len);


//------------------------------------------------------------------------
// This function opens the output file and creates a tree in it.
//------------------------------------------------------------------------
void initroot_(const char *configFilename, int lconfigFilename)
{
  const char* cfgName = strFtoC(configFilename, lconfigFilename);

  eventHandler = new PSTree(std::string(cfgName));
  eventHandler->Initialise();
}//initroot_

//------------------------------------------------------------------------
// This function retrieves the four-momentum and PDG code
// for the particle current placed in the common block.
// It then adds the variables to the respective vectors.
//------------------------------------------------------------------------
void addparticle_()
{
  eventHandler->AddParticle(particle_.id, particle_.px, particle_.py, particle_.pz, particle_.e);
}//addparticle_

//------------------------------------------------------------------------
// This function fills the tree branches and resets the
// vectors ready for a new event.
//------------------------------------------------------------------------
void fillroot_()
{
  eventHandler->SetScale(event_.q2ren);
  eventHandler->SetPdfInfo(event_.fl1, event_.x1, event_.fl2, event_.x2, event_.q2fact);
  eventHandler->SetWeight(event_.wgt);
  eventHandler->AddEvent();
}//fillbranches_

//------------------------------------------------------------------------
// This function writes the data to the output file.
//------------------------------------------------------------------------
void writeroot_()
{
  eventHandler->Write();
}//writeroot_

//------------------------------------------------------------------------
// This function writes the data to file and closes it.
//------------------------------------------------------------------------
void closeroot_()
{
  eventHandler->Close();
}//closeroot_

//------------------------------------------------------------------------
// Added for compatibility with MC@NLO
//------------------------------------------------------------------------
void rclos_()
{
  closeroot_();
}//rclos_

//------------------------------------------------------------------------
// Converts a Fortran string to a C string (adds '\0' character at end.)
//------------------------------------------------------------------------
char* strFtoC(const char *str, int len)
{
  int tlen=0;
  char tem;

  // Counts non-blank charachters in a string str until a first blank character or the end of the string is met
  while(str[tlen] !=' ' && (tlen < len ) && (tem=str[tlen++],tem));


  char *tstr = new char[tlen+1];
  strncpy(tstr,str,tlen);
  tstr[tlen]='\0';

  return tstr;
}//strFtoC

