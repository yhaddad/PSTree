#ifndef fInterface_H
#define fInterface_H

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


char* strFtoC(const char *str, int len);
void initroot_(const char *configFilename="", int lconfigFilename=0);
void addparticle_();
void fillroot_();
void writeroot_();
void closeroot_();
void rclos_();

#endif
