#ifndef PSTree_H
#define PSTree_H

// Standard libraries
#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <string>

// ROOT and FastJet libraries
#include <TFile.h>
#include <TTree.h>
#include "fastjet/ClusterSequence.hh"

#include "INIReader.h"

class PSTree{
public:
	// Constructors and destructors
	//PSTree();
	PSTree(std::string configFilename = "");
	~PSTree();

	void Initialise();
	void AddParticle(int PDGCode, double px, double py, double pz, double e);
	void SetWeight(double weight);
	void SetScale(double Q2ren);
	void SetPdfInfo(int flav1, double x1, int flav2, double x2, double Q2fac);
	void AddEvent();
	void Write();
	void Close();

private:
	// Private methods
	void DeclareBranches();
	void FindJets();
	void FindLeptons();
	void FillBranches();
	void ClearVectors();
	void WriteCurrentEventToFile();
	int ParseConfigFile(std::string configFilename);
	void PrintConfig();

	// ROOT file variables
	std::string m_filename, m_treename;
	TFile *m_file;
	TTree *m_tree;

	// Options
	bool m_isOutputHadrons, m_isOutputJets;
	bool m_isOutputPdfInfo;
	bool m_isDebug;

	// Object definition variables
	double m_leptonPtCut;
	double m_jetPtCut, m_jetDeltaR;
	fastjet::JetAlgorithm m_jetAlg;
	fastjet::JetDefinition *m_jetDef;
	std::set<int> m_InvisibleParticles, m_RetainedParticles;

	// Event variables
	double m_weight;
	double m_Q2ren, m_x1, m_x2, m_Q2fac, m_flav1, m_flav2;
	int m_nParticles, m_nJets, m_nEvents;
	std::vector<int> m_partID, m_lepID;
	std::vector<double> m_partPx, m_partPy, m_partPz, m_partE;
	std::vector<double> m_jetPx,  m_jetPy,  m_jetPz,  m_jetE;
	std::vector<double> m_lepPx,  m_lepPy,  m_lepPz,  m_lepE;
};

#endif
