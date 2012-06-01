#include "PSTree.h"

//------------------------------------------------------------------------------------
// 
//                                  Public methods
// 
//------------------------------------------------------------------------------------
PSTree::PSTree(std::string configFilename) :
m_filename("output.root"), m_treename("tree"),
m_isOutputHadrons(false), m_isOutputJets(true), m_isOutputPdfInfo(true), m_isDebug(false),
m_leptonPtCut(5.0), m_jetPtCut(15.0), m_jetDeltaR(0.4),m_jetAlg(fastjet::antikt_algorithm),
m_jetDef(NULL)
{
	if (!ParseConfigFile(configFilename)){
		std::cout << "PSTree:: Error: Failed to read configuration file " << configFilename;
		std::cout << ". Using default values." << std::endl;
	}
}//Default constructor

void PSTree::Initialise()
{
	PrintConfig();

	m_file = new TFile(m_filename.c_str(), "RECREATE");
	if (!m_file){
		std::cout << "PSTree:: Error: Cannot open ROOT file" << std::endl;
		return;
	}
	m_tree = new TTree(m_treename.c_str(), m_treename.c_str());
	if (!m_tree){
		std::cout << "PSTree:: Error: Cannot create ROOT tree" << std::endl;
		return;
	}

	m_nEvents = 0;
	DeclareBranches();
	m_jetDef = new fastjet::JetDefinition(m_jetAlg, m_jetDeltaR);

	m_InvisibleParticles.insert(12);
	m_InvisibleParticles.insert(-12);
}//Initialise

void PSTree::AddParticle(int PDGCode, double px, double py, double pz, double e)
{
	m_partID.push_back(PDGCode);
	m_partPx.push_back(px);
	m_partPy.push_back(py);
	m_partPz.push_back(pz);
	m_partE.push_back(e);	
}//AddParticle

void PSTree::AddEvent()
{
	m_nEvents++;
	m_nParticles = (int)m_partID.size();
	m_nJets = 0;

	// Check vectors sizes consistent
	if ((int)m_partID.size() != m_nParticles ||
		(int)m_partPx.size() != m_nParticles ||
		(int)m_partPy.size() != m_nParticles ||
		(int)m_partPz.size() != m_nParticles ||
		(int)m_partE.size()  != m_nParticles)
		std::cout << "PSTree:: Warning: Inconsistent vector sizes" << std::endl;			

	// If jet finding is failing, write event to file beforehand for inspection
	if (m_isDebug){
		WriteCurrentEventToFile();
	}

	FindJets();
	FindLeptons();
	FillBranches();
	ClearVectors();
}//AddEvent

void PSTree::SetScale(double Q2ren)
{
  m_Q2ren = Q2ren;
}//SetScale

void PSTree::SetPdfInfo(int flav1, double x1, int flav2, double x2, double Q2fac)
{
	m_flav1 = flav1;
	m_x1    = x1;
	m_flav2 = flav2;
	m_x2    = x2;
	m_Q2fac = Q2fac;
}//SetPdfInfo

void PSTree::SetWeight(double weight)
{
	m_weight = weight;
}//SetWeight

void PSTree::Write()
{
	if (m_file){
		m_file->Write();
	}
	else{
		std::cout << "PSTree:: Error: No ROOT file was opened" << std::endl;
	}
}//Write

void PSTree::Close()
{
	if (m_file){
		m_tree->Write();
		m_file->Close();
		delete m_file;
	}
	else{
		std::cout << "PSTree:: Error: No ROOT file was opened" << std::endl;
	}
}//Close



//------------------------------------------------------------------------------------
// 
//                                  Private methods
// 
//------------------------------------------------------------------------------------
void PSTree::DeclareBranches()
{
	// Give tree addresses of variables
	m_tree->Branch("weight",     &m_weight);
	m_tree->Branch("nParticles", &m_nParticles);
	m_tree->Branch("nJets",      &m_nJets);

	m_tree->Branch("lepID", &m_lepID);
	m_tree->Branch("lepPx", &m_lepPx);
	m_tree->Branch("lepPy", &m_lepPy);
	m_tree->Branch("lepPz", &m_lepPz);
	m_tree->Branch("lepE",  &m_lepE);

	m_tree->Branch("Q2ren", &m_Q2ren);

	if(m_isOutputPdfInfo){
		m_tree->Branch("flav1", &m_flav1);
		m_tree->Branch("x1",    &m_x1);
		m_tree->Branch("flav2", &m_flav2);
		m_tree->Branch("x2",    &m_x2);
		m_tree->Branch("Q2fac",    &m_Q2fac);
	}

	if(m_isOutputHadrons){
		m_tree->Branch("partID", &m_partID);
		m_tree->Branch("partPx", &m_partPx);
		m_tree->Branch("partPy", &m_partPy);
		m_tree->Branch("partPz", &m_partPz);
		m_tree->Branch("partE",  &m_partE);
	}

	if(m_isOutputJets){
		m_tree->Branch("jetPx", &m_jetPx);
		m_tree->Branch("jetPy", &m_jetPy);
		m_tree->Branch("jetPz", &m_jetPz);
		m_tree->Branch("jetE",  &m_jetE);
	}
}//DeclareBranches

void PSTree::FindJets()
{
	std::vector<fastjet::PseudoJet> particles;

	// Include all final state particles except neutrinos
	for(int i=0; i<m_nParticles; i++){
		if( fabs(m_partID[i]) != 12 &&
			fabs(m_partID[i]) != 14 &&
			fabs(m_partID[i]) != 16)
			particles.push_back(fastjet::PseudoJet(m_partPx[i], m_partPy[i], m_partPz[i], m_partE[i]));
	}

 	fastjet::ClusterSequence clustSeq(particles, *m_jetDef);

  	std::vector<fastjet::PseudoJet> jets = sorted_by_pt(clustSeq.inclusive_jets(m_jetPtCut));

  	m_nJets = (int)jets.size();
  	for(int i=0; i<m_nJets; i++){
    	m_jetPx.push_back(jets[i].px());
    	m_jetPy.push_back(jets[i].py());
    	m_jetPz.push_back(jets[i].pz());
    	m_jetE.push_back(jets[i].E());
  	}
}//FindJets

void PSTree::FindLeptons()
{
	for(int i = 0; i < m_nParticles; i++){
		// Select all lepton flavours
		if( m_partID[i] == 11 || m_partID[i] == -11 ||
			m_partID[i] == 12 || m_partID[i] == -12 ||
			m_partID[i] == 13 || m_partID[i] == -13 ||
			m_partID[i] == 14 || m_partID[i] == -14 ||
			m_partID[i] == 15 || m_partID[i] == -15 ||
			m_partID[i] == 16 || m_partID[i] == -16 ){

			// Use a loose lepton pT cut to ignore
			// leptons produced in showering
			double pT = sqrt((m_partPx[i] * m_partPx[i]) + (m_partPy[i] * m_partPy[i]));
			if(pT > m_leptonPtCut){
				m_lepID.push_back(m_partID[i]);
				m_lepPx.push_back(m_partPx[i]);
				m_lepPy.push_back(m_partPy[i]);
				m_lepPz.push_back(m_partPz[i]);
				m_lepE.push_back(m_partE[i]);
			}
		}
	}
}//FindLeptons

void PSTree::FillBranches()
{
	m_tree->Fill();
}//FillBranches

void PSTree::ClearVectors()
{
	m_partID.clear();
	m_partPx.clear();
	m_partPy.clear();
	m_partPz.clear();
	m_partE.clear();

	m_jetPx.clear();
	m_jetPy.clear();
	m_jetPz.clear();
	m_jetE.clear();

	m_lepID.clear();
	m_lepPx.clear();
	m_lepPy.clear();
	m_lepPz.clear();
	m_lepE.clear();
}//ClearVectors

void PSTree::WriteCurrentEventToFile()
{
	ofstream output("PSTree_debug.txt", std::ios::out | std::ios::trunc);
	if (output.is_open()){
		for (int i=0; i<m_nParticles; i++){
			output << m_partID[i] << "\t"
			<< m_partPx[i] << "\t"
			<< m_partPy[i] << "\t"
			<< m_partPz[i] << "\t"
			<< m_partE[i]  << "\t" << std::endl;
		}
	}
	output.close();
}//WriteCurrentEventToFile

int PSTree::ParseConfigFile(std::string configFilename)
{
	INIReader reader(configFilename);
	if (reader.ParseError() < 0)
		return 0;

	m_filename = reader.Get("File", "filename", "output.root");
	m_treename = reader.Get("File", "treename", "tree");

	m_isDebug  = reader.GetBoolean("Options", "debug", false);
	m_isOutputJets = reader.GetBoolean("Options", "outputJets", true);
	m_isOutputHadrons = reader.GetBoolean("Options", "outputHadrons", false);
	m_isOutputPdfInfo = reader.GetBoolean("Options", "outputPdfInfo", true);

	m_leptonPtCut = reader.GetDouble("Object definition", "Lepton pT cut", 5.0);
	m_jetPtCut = reader.GetDouble("Object definition", "Jet pT cut", 15.0);
	m_jetDeltaR = reader.GetDouble("Object definition", "Jet DeltaR", 0.4);
	std::string jetAlgorithm = reader.Get("Object definition", "Jet algorithm", "AntiKt");
	if (jetAlgorithm == "AntiKt")
		m_jetAlg = fastjet::antikt_algorithm;
	else if (jetAlgorithm == "Kt")
		m_jetAlg = fastjet::kt_algorithm;
	else if (jetAlgorithm == "Cambridge" || jetAlgorithm == "Aachen" || jetAlgorithm == "Cambridge/Aachen")
		m_jetAlg = fastjet::cambridge_algorithm;
	else
		std::cout << "PSTree:: Error: Unsupported jet algorithm input." << std::endl;

	return 1;
}//ParseConfigFile

void PSTree::PrintConfig()
{
	std::cout << std::endl;
	std::cout << "PSTree was run with the following input parameters:" << std::endl;
	std::cout << "Filename: " << m_filename << std::endl;
	std::cout << "Treename: " << m_treename << std::endl;
	std::cout << "Debug: " << m_isDebug << std::endl;

	std::cout << "Object definitions" << std::endl;
	std::cout << "Lepton pT cut: " << m_leptonPtCut << std::endl;
	std::cout << "Jet pT cut: "    << m_jetPtCut    << std::endl;
	std::cout << "Jet DeltaR: "    << m_jetDeltaR   << std::endl;
	std::string jetAlgorithm;
	if (m_jetAlg == fastjet::antikt_algorithm)
		jetAlgorithm = "AntiKt";
	else if (m_jetAlg == fastjet::kt_algorithm)
		jetAlgorithm = "Kt";
	else if (m_jetAlg == fastjet::cambridge_algorithm)
		jetAlgorithm = "Cambridge/Aachen";
	std::cout << "Jet algorithm: " << jetAlgorithm << std::endl;
	std::cout << std::endl;
}
