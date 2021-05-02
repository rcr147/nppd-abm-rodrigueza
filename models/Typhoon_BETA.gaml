/***
* Name: TyphoonBETA
* Author: reyrodrigueza
* Description: A model for typhoon evacuation behavior
* Tags: Tag1, Tag2, TagN
***/

model TyphoonBETA

/* Insert your model definition here */


global 
{
	float step <- 20 #s;				//a step represents 10 seconds
	
	int nb_rescuers_init <- 15;			//initial number of rescuers
	int nb_households_init <- 10;		//initial number of households
	int nb_shelterManagers_init <- 4;	//initial number of shelter managers

	
	float hhPerceptionDistance <- 50#m;		//household perception distance
	float resPerceptionDistance <- 50#m;	//rescuer perception distance
	float sheltPerceptionDistance <- 50#m;	//shelter manager perception distance
	
	int evacuated_inside <- 0;			//number of households who evacuated within the barangay
	int evacuated_outside <- 0;			//number of households who evacuated outside the barangay
	int total_evacuees <- 0;			//total number of evacuees
	int availableEvacuationCenters;
	int availableVolunteerCenters; 
	
	int rescuersBackToStartingPoint <- 0;
	
	float init_probability_cooperation <- 0.5;
	
//	float floodHeight_init <- 0.0;
//	float floodWaterFlowSpeed <- 0.0;
	
	// HAZARD-RELATED FACTORS
	int stormSeverity;					//storm signal [1,2,3,4,5]
	string rainfallSeverity;			//rainfall classification [yellow,orange,red]
	float proximityToHazard <- 0.0;		//distance from household location to source of hazard
	//float sourceOfEvacuationWarning <- 0.0;
	string timeOfDay <- "Daytime";		//day or night
	float tideLevel <- 0.0;				// NOT YET USED 
	float _stormSeverity;
	float _rainfallSeverity;
	float _timeOfDay;
	
	bool directed <- true;		// whether disaster manager commands rescuers 
	
	date catastrophe_date;
	float time_before_hazard <- 1#h ;
	
	//DECISION FACTORS WEIGHT
	float weight_CDM <- 0.0;					//weight for characteristics of decision maker
	float weight_HRF <- 0.0;					//weight for house-related factors
	float weight_CRF <- 0.0;					//weight for capacity-related factors
	
	float hhHighestPossibleScore;		//highest perceived risk score for a household
	
	// SHAPEFILES
	file road_file <- file("../includes/roads.shp");
	file buildings <- file("../includes/buildings.shp");
	file water_body <- file("../includes/waterways.shp");
	file points <- file("../includes/points.shp");
	file evacuationPoints <- file("../includes/evacuationPoints.shp");
	file officialEvacuationCenters <- file("../includes/OfficialEvacuationCenters.shp");
	file additionalShelters <- file("../includes/VolunteerShelters.shp");
	file destinationPoints <- file("../includes/destinationPoints.shp");
	file floodProneAreas <- file("../includes/floodProneAreas.shp");
	
	geometry shape <- envelope(envelope(road_file)+envelope(water_body));
	//geometry shape <- envelope(rectangle);
		
	graph<geometry, geometry> road_network;
	graph<geometry, geometry> rescuers_road_network;
	graph<geometry, geometry> households_road_network;
	
	map<road,float> road_weights;
	
	//SPECIES SHUFFLED
	list<building> buildingShuff;
	list<rescuers> rescuersShuff;
	list<households> householdsShuff;
	list<shelterManagers> managersShuff;
	list<rescuersStartingPoints> rescuersStartingPointsShuff;
	list<volunteerShelters> volunteerSheltersShuff;
	list<evacuationCenters> evacuationCentersShuff;
	
	int xbounds <- int(shape.width/10); 
	int ybounds <- int(shape.height/10); 
	int xmin <- xbounds;   
	int ymin <- ybounds;  
	int xmax <- int(shape.width - xbounds);     
	int ymax <- int(shape.height - ybounds);
	
	float x <- 0.0;				//cooperator_totalNeighbor_ratio;
	float D <- 2*x - 0.5;		//payoff curve for defectors
	float C <- 2*x - 1.0;		//payoff curve for cooperators

	init 
	{
		//list<geometry> clean_lines <- clean_network(road_file.contents, 5, true, true);
		list<geometry> clean_roads <- clean_network(road_file.contents,0.0,true,false); //thanks to Dr.Kevin Chapuis for this line
		create road from:clean_roads;
		create building from:buildings;
		create hazard from: water_body;
		
		//create rescuersStartingPoints from: points; ORIGINAL
		create rescuersStartingPoints from: points
		{
			rescuerIsBackToStartingPoint <- false;  //ADDED
		}
		create householdsEvacuationPoints from: evacuationPoints;
		
		create rescuersDestinationPoints from: destinationPoints
		{
			reached <- false;
		}
		
		
		create evacuationCenters from: officialEvacuationCenters with: [evacuationCenterCapacity::string(get("capacity"))]
		{
			evacCenterManager <- shelterManagers closest_to(self); 
			acceptedEvacuees <- 0;
			full <- false;
		}
		
		create volunteerShelters from: additionalShelters with: [volunteerShelterCapacity::string(get("capacity"))]
		{
			acceptedVEvacuees <- 0;
			full <- false;
		}
		
		create floodproneAreas from: floodProneAreas with: [floodRisk::string(get("name"))]
		{
			//floodRisk <- nil;
		}
		
		//AGENTS' ROAD NETWORK
		households_road_network <- as_edge_graph(road);
		rescuers_road_network <- as_edge_graph(road);
		road_network <- as_edge_graph(road);
		road_weights <- road as_map (each::each.shape.perimeter);
		
		//SPECIES SHUFFLED
		buildingShuff <- shuffle(building);
		rescuersShuff <- shuffle(rescuers);
		householdsShuff <- shuffle(households);
		managersShuff <- shuffle(shelterManagers);
		rescuersStartingPointsShuff <- shuffle(rescuersStartingPoints);
		volunteerSheltersShuff <- shuffle(volunteerShelters);
		evacuationCentersShuff <- shuffle(evacuationCenters);
		
		availableEvacuationCenters <- length(evacuationCentersShuff);
		availableVolunteerCenters <- length(volunteerShelters);
		
		
		create households from:csv_file( "../includes/householdAgents.csv",true) with:
			[hhID::int(get("HouseholdID")), 
				headOfHousehold::string(get("HeadOfHousehold")), 
				incomeLevel::string(get("IncomeLevel")),
				levelOfEducation::string(get("LevelOfEducation")), 
				hasSmallKids::bool(get("PresenceOfChildren")),
				hasElderly::bool(get("PresenceOfElderly")),
				withDisability::bool(get("PresenceOfDisabledMembers")),
				houseOwnership::bool(get("HouseOwnership")),
				yearsOfResidency::bool(get("YearsOfResidency")),
				pastTyphoonExperience::bool(get("TyphoonExperience"))
			]	
			{	//thanks to Dr.Alexis Drogoul for this part
				choice <- first(buildingShuff);
				remove choice from: buildingShuff;
				location <- any_location_in(choice);
				speed <- rnd(2, 5)#km/#h;
				//location <- any_location_in(one_of(building));
			}
			
			
//		list<string> names <- households collect each.name; // each is of type my_species
			
		
//		create households number:nb_households_init 
//		{	
//			location <- any_location_in(one_of(building));
////			location <- one_of(building);
////			safety_point <- evacuation_point with_min_of (each distance_to self);
//		}		
		
		create rescuers number:nb_rescuers_init
		{
//			rescuer <- first(rescuersShuff);	
//			startingPoint <- first(rescuersStartingPointsShuff);
//			remove startingPoint from: rescuersStartingPointsShuff;
//			location <- any_location_in(startingPoint);

			location <- any_location_in(one_of(points));
			startPoint <- location;
//			evacuation_point start <- any_location_in(one_of(building));
//			location <-start.location;
//			capacity <- nb_capacity;
//			home <- start;
		}
				
		create shelterManagers number:nb_shelterManagers_init
		{
			evacCenter <- first(evacuationCentersShuff);
			remove evacCenter from: evacuationCentersShuff;
			location <- any_location_in(evacCenter);
			//capacity <- int(evacCenter.evacuationCenterCapacity); // deadlock code
			acceptedEvacuees <- 0;
			full <- false;
		}		
		
		
		
			
//		create MDRRMO;

//		//CONVERTING INPUT VALUES 
//		if(rainfallSeverity = "Yellow") 		{	_rainfallSeverity <- 0.4;	}
//		else if(rainfallSeverity = "Orange")	{	_rainfallSeverity <- 0.7;	}
//		else if(rainfallSeverity = "Red")		{	_rainfallSeverity <- 1.0;	}
//		
//		if(timeOfDay = "Day time")			{	_timeOfDay <- 0.5;	}
//		else if(timeOfDay = "Night time")	{	_timeOfDay <- 1.0;	}
//		
//		switch stormSeverity 
//		{
//			match 1	{	_stormSeverity <- 0.4;	}
//			match 2 {	_stormSeverity <- 0.7;	}
//			match 3	{	_stormSeverity <- 1.0;	}
//			default {	_stormSeverity <- 1.0;	}
//		}
	}
		
//  reflex countAvailableEvacuationCenters when: !empty(evacuationCenters)
//	{
//		//TODO 
//		loop EC over: evacuationCenters
//		{
//			if(EC.full) 
//			{ 
//				remove EC from: evacuationCenters;
//				availableEvacuationCenters <- availableEvacuationCenters - 1;
//				write "availableEvacuationCenters:" + availableEvacuationCenters;
//			}
//		}
//	} 
	
	
//	reflex halting when: empty (rescuersDestinationPoints) // WHEN NO ONE EVACUATES ANYMORE AFTER N TIME DURING SIMULATION
//	{
//		do halt;
//	}
	
//	action recompute {
//		road_network <- as_edge_graph(road where !each.is_flooded);
//		using topology(road_network) {
//			ask inhabitant where !each.is_evacuated {
//				safety_point <- evacuation_point closest_to self;
//			}
//		}
//	}
	
//	reflex halting when: (rescuersBackToStartingPoint = nb_rescuers_init)//rescuers backToStartingPoint ) //or WHEN NO ONE EVACUATES ANYMORE AFTER N TIME DURING SIMULATION
//	{
//		do halt;
//	}

	
}
	
species households skills:[moving] 
{
	int hhID <- 0;		// agent ID
	
	building choice;
	bool alerted <- false;
	bool wander <- false;
	bool stay <- true;
	bool evacuateNow <- false;
	
	float speed;
	//float perceptionDistance <- 50#m;
	
	//list<building> neighbors update: building at_distance vicinity_distance;
	
	/* DECISION FACTORS */
	
	// CHARACTERISTICS OF DECISION MAKER
	string headOfHousehold;
	string incomeLevel;
	string levelOfEducation;
	bool hasSmallKids;
	bool hasElderly;
	bool withDisability;
	bool houseOwnership;
	bool yearsOfResidency;
	
	
	// HAZARD-RELATED FACTORS
	// This also includes stormSeverity, rainfallSeverity, proximityToHazard 
	// which are declared as global variables
	string sourceOfEvacuationWarning;
	float _sourceOfEvacuationWarning;	// could be rescuers, family/friends or media
	
	
	// CAPACITY-RELATED FACTORS
	string houseQuality <- nil;
	float floorLevels <- 0.0;
	bool pastTyphoonExperience <- nil;
	// float houseDamage <- 0.0;			TO BE COMPUTED
	
	// PERCEIVED RISK
	float vitality <- 0.0;
	float perceivedRisk <- 0.0;
	float RISK <- 0.0;
	
	// DECISION FACTORS TOTAL VALUES
	float CDM <- 0.0;
	float HRF <- 0.0;
	float CRF <- 0.0;
	
	//float cooperator_totalNeighbor_ratio <- 0.0;
	
	// NEIGHBORS
	int totalNeighborhood <- 0;
	//list<households> neighbours <- shuffle(households.near);
 
	
	//Graph roadNetwork
	//float priority;
	float size;
	
	rgb color <- rgb(255,255,179);
	
	init
	{
		do compute_CharacteristicsOfDecisionMaker();
		do compute_CapacityRelatedFactors();
		
		// SOURCE OF EVACUATION WARNING
		if(flip(0.5))	{	_sourceOfEvacuationWarning <- 0.4;		}	//friends as source of evacuation warning
		else			{	_sourceOfEvacuationWarning <- 0.7;		}	//media as source of evacuation warning
		/* Households' source of evacuation warning score becomes 1.0 if they are warned by disaster manager - authorities */
		
		// 
		if(flip(0.3))	{	floorLevels <- 0.5;		}	//the building/house has more than 1 floor levels
		else			{	floorLevels <- 1.0;		}	//the building/house has 1 floor level
	}
	
	
	/* BEHAVIORS */
//	reflex stay when: !evacuateNow
//	{
//		//DO NOT MOVE ANYMORE
//	}
	
	reflex wander when: alerted or wander 
	{
		// Wanders around building when not evacuating.//SHOULD BE removed if it consumes a lot of memory!
		if(flip(0.05))
		{
			do wander speed: speed amplitude: 0.5 bounds: choice;
		}
	}
	
	reflex listNeighbors 
	{
		// Neighbors contains the list of all the household agents within the perception 
		// distance of the caller household agent.
		list<households> neighbors <- households at_distance(hhPerceptionDistance);
	}
	
	reflex findNearestRescuer
	{
		list<rescuers> nearRescuers <- rescuers at_distance(hhPerceptionDistance+(hhPerceptionDistance/2));
		
		rescuers priority <- nearRescuers closest_to(self);
		// priority is the rescuer that is nearest to the household agent
	}
	
	reflex compute_HazardRelatedFactors
	{
		//float sourceOfEvacuationWarning <- 		RANDOM
		
		//proximityToHazard <- calculate_proximityToHazard(self.location distance_to hazard[0]);	
		
		proximityToHazard <- calculate_proximityToHazard(self.location distance_to first(hazard));	
		
		HRF <- (stormSeverity + _rainfallSeverity + proximityToHazard + 
			_sourceOfEvacuationWarning + _timeOfDay);
			
		//write proximityToHazard;
		//write "hhID: " + hhID + "   HRF: " + HRF;
	}
	
	reflex compute_PerceivedRisk
	{
		float boundedRationality <- rnd (0.0, 0.05); 	// between 0.0 and 0.05  
		
		//COMPUTE RISK ACCORDING TO THE HOUSEHOLD AGENT'S PERCEPTION
		perceivedRisk <- ((CDM * weight_CDM) + (HRF * weight_HRF) + (CRF * weight_CRF)) + boundedRationality; 
		
		hhHighestPossibleScore <- 8*weight_CDM + 3*weight_CRF + 5*weight_HRF;
		
		// IF HOUSEHOLD'S PERCEIVED RISK IS HIGHER THAN 70% OF THE HIGHEST POSSIBLE PERCEIVED RISK SCORE, THEN EVACUATE
		if(perceivedRisk > hhHighestPossibleScore * 0.7)	{	evacuateNow <- true;	}
		
		//COMPUTE RISK ACCORDING TO DISASTER MANAGER
		RISK <- (HRF * CDM) / CRF; //max: (8+5)/1.4 = 9.285 (worst-case)   min: (2.3+2.1)/3.0 = 1.466 (best-case)
		
		
		write "hhID: " + hhID + 
			  "\tRISK: " + floor(RISK) + 
			  "\tpRisk: " + floor(perceivedRisk) + 
			  "\thpScore: " + floor(hhHighestPossibleScore); 
	}
	
	// Evacuate to evacuation centers inside the barangay
	reflex evacuateToEC when: evacuateNow and availableEvacuationCenters > 0	//FIX the condition
	{
		// Look for nearby possible evacuation centers when alerted and evacuation centers inside
		// the barangay are not full.  
		// When not evacuating, wander inside shelters. 
		
		householdsEvacuationPoints evacuation_point;
		evacuationCenters evacuation_center;
		volunteerShelters  volunteer_shelter;
			
		using topology(road_network) 
		{
			evacuation_center <- evacuationCenters with_min_of(each distance_to self);	
			volunteer_shelter <- volunteerShelters  with_min_of(each distance_to self);				
		}
		
		do goto target: evacuation_center on: road_network;
		
			 
	}
	
	// Evacuate to evacuation centers outside the barangay
	reflex evacuateToEP when: evacuateNow and availableEvacuationCenters = 0
	{
		// Look for nearby possible evacuation point when alerted and when evacuation centers inside 
		// the barangay are full.
		// When not evacuating, wander.
		householdsEvacuationPoints evacuation_point;
		
		using topology(road_network) 
		{
			evacuation_point <- householdsEvacuationPoints with_min_of(each distance_to self);			
		}
		
		do goto target: evacuation_point on: road_network;	
	}
	
	reflex updateInputValues
	{
		//CONVERTING INPUT VALUES : These input values are from sliders for choosing the severity of 
		// typhoon, rainfall and time of day. 
		if(rainfallSeverity = "Yellow") 		{	_rainfallSeverity <- 0.25;	}  // YELLOW rainfall classification //0.4; {7.5-15 mm rain}
		else if(rainfallSeverity = "Orange")	{	_rainfallSeverity <- 0.5;	}  // ORANGE rainfall classification //0.7; {15-30 mm rain}
		else if(rainfallSeverity = "Red")		{	_rainfallSeverity <- 1.0;	}  // RED rainfall classification	    	{>30 mm rain}
		
		if(timeOfDay = "Day time")			{	_timeOfDay <- 0.5;	}
		else if(timeOfDay = "Night time")	{	_timeOfDay <- 1.0;	}
		
		switch stormSeverity 
		{
			match 1	
			{	
				if(incomeLevel = "middle" or incomeLevel = "high")	{	_stormSeverity <- 0.25;	} //0.4;
				else												{	_stormSeverity <- 0.5;	} //0.7;
			}  // Typhoon Signal #1
			match 2 
			{	
				if(incomeLevel = "middle" or incomeLevel = "high")	{	_stormSeverity <- 0.5;	} //0.7;
				else												{	_stormSeverity <- 1.0;	}
			}  // Typhoon Signal #2
			match 3	{	_stormSeverity <- 1.0;	}  // Typhoon Signal #3
			default {	_stormSeverity <- 1.0;	}  // Typhoon Signal #4-5
		}
	}
	
	
	
	/* ACTIONS */
	
	// This action (method/function/subroutine) computes the value for the characteristics of the decision maker (CDF) 
	// in the household (head of household). The computation is done one-time only.
	action compute_CharacteristicsOfDecisionMaker
	{
		float _headOfHousehold <- headOfHousehold = "male" ? 0.5: 1.0; 
		float _incomeLevel <- nil; 
		float _levelOfEducation <- nil;
		float _hasSmallKids <- hasSmallKids = true ? 1.0: 0.0;
		float _hasElderly <- hasElderly = true ? 1.0: 0.0;
		float _withDisability <- withDisability = true ? 1.0: 0.0;
		float _houseOwnership <- houseOwnership = true ? 0.5: 1.0;
		float _yearsOfResidency <- yearsOfResidency = true ? 0.5: 1.0;
		
		
		if(incomeLevel = "high")		{	_incomeLevel <- 0.25;	} //0.4
		else if(incomeLevel = "middle")	{	_incomeLevel <- 0.5;	} //0.7
		else if(incomeLevel = "low")	{	_incomeLevel <- 1.0;	}
		
		if(levelOfEducation = "college")			{	_levelOfEducation <- 0.25;	} //0.4
		else if(levelOfEducation = "high school")	{	_levelOfEducation <- 0.5;	} //0.7
		else if(levelOfEducation = "elementary")	{	_levelOfEducation <- 1.0;	}
	
		CDM <- (_headOfHousehold + _incomeLevel + _levelOfEducation + _hasSmallKids + _hasElderly+
			_withDisability + _houseOwnership + _yearsOfResidency);
	}
	
	// This action (method/function/subroutine) computes the capacity-related factors (CRF) value for each 
	// household. It includes housing-related data and past typhoon experience. The computation is done one-time only.
	action compute_CapacityRelatedFactors	//one-time computation
	{
		float _houseQuality <- 0.0;	//concrete=39%, wood/concrete+wood=38%, light materials=23%
		float _floorLevels <- floorLevels > 1 ? 0.5: 1.0;
		float _pastTyphoonExperience <- pastTyphoonExperience = true ? 0.5: 1.0;
		
		if(houseQuality = "concrete")	{	_houseQuality <- 0.25;	} //0.4
		else if(houseQuality = "wood")	{	_houseQuality <- 0.5;	} //0.7
		else							{	_houseQuality <- 1.0;	}
		
		CRF <- (_houseQuality + _floorLevels + _pastTyphoonExperience);
	}
	
	
	float calculate_proximityToHazard(float distance)
	{
		if(distance > 40#m)			{	return 0.25;	}	// no-build zone is 40 m
		else if(distance > 20#m)	{	return 0.5;	}	// near source of hazard
		else						{	return 1.0;	}	// within the source of hazard
	}
	
	aspect default 
	{	
		//draw circle(3#m) color: alerted ? #red : #orange;
		draw circle(3#m) color: evacuateNow ? #white : (alerted ? #violet : #orange);
//		draw circle(hhPerceptionDistance) color: #cyan empty: true; //perception distance
	}
	
	/*
	 * For PHASE II - N-Person Prisoner's Dilemma -----------------------------------------------------------------
	 */
	action askForHelp {}	
	action helpNeighbor {}	
	action cooperate {}
	action defect {}
	//compute_vitality
	//die()
	
	
	//// --------------------EQUATIONS -------------------
//// Weighted Payoff
//// given a household agent, 
 
//float RP_wt <- 0.0; 
//loop i from:1 to: 3 
//{
//	RP_wt <- RP_wt + W(i)*Mc(i);
//}
	//where W_i is a weighting parameter such that 
	// all weights sum to one, and Mc_i is the history payoff
	// (Mc_1 stores the current payoff. This assumes that 
	// the effects of memory decrease with time, W_1 >= W_2 >= W_3. 

//// Updating Scheme

//if((S(t) = S(t-1)) and (S(t-1) = S(t-2)))
//	alpha_i(t+1) <- alpha_i(t) + 0.15;
//else if((S(t) = S(t-1)) and (S(t-1) != S(t-2)))
//	alpha_i(t+1) <- alpha_i(t) + 0.10;
//else if(S(t) != S(t-1))
//	alpha_i(t+1) <- alpha_i(t) - 0.10;
	
//// Probability of Cooperation for household agent i at time t+1
//// given time t,

//if (S(t) = C and RP_wt > 0)
//	p(t+1) <- p(t) + (1-p(t)) * alpha_i;
//else if (S(t) = C and RP_wt <= 0)
//	p(t+1) <- (1-alpha_i) * p(t);

//// for every t, there must be q(t) = 1 - p(t)
//// given time t,

//if (S(t) = D and RP_wt > 0)
//	q(t+1) <- q(t) + (1-q(t)) * alpha_i;
//else if (S(t) = D and RP_wt <= 0)
//	q(t+1) <- (1-alpha_i) * q(t);

//// Neighborhood Production Function
//// for time t, PF is the cooperation payoff for the group

//int N <- neighborhood(j);
//PF(t) <- 0.0;
//loop j from:1 to: N	{	PF(t) <- PF(t) + C(j);	}
//PF(t) <- PF(t) / N;

	// where C_j is the payoff value for the agent j, 
	// and N is the total number of agents in the neighborhood
							
//// Average Neighborhood Function for three memory events
//float PFavg <- 0.0;
//loop i from:1 to: 3		{	PFavg <- PFavg + PF(i);		} 
//PFavg <- PFavg / 3;

	
//// State of agent i at time t+1 with S(t)
//// for S(t) = C,

//if (RP_wt for agent i < PF_avg and p(t+1) < q(t+1) and q(t+1) > R_u)
//	S(t+1) <- D;
//else 
//	S(t+1) <- C;	// retain previous action if the conditions for D are not satisfied
	
//// for S(t) = D,

//if (RP_wt for agent i < PF_avg and q(t+1) < p(t+1) and p(t+1) > R_u)
//	S(t+1) <- C;
//else 
//	S(t+1) <- D;	// retain previous action if the conditions for C are not satisfied
	
// where R_u is a uniform random value between 0 and 1
//// ----------------------------------------------------------------------------------------------------
	
}


species rescuers skills:[moving] 
{
	bool alerted <- false;
	bool backToStartingPoint <- false;
//	bool directed <- true;
	int capacity <- 0;
	bool fullCapacity <- false;
	int sensedHouseholds <- 0;
	int sensedFellowRescuers <- 0;
	float proximityToHazard <- 0.0;
	float vitality <- 0.0;
	float perceivedRisk <- 0.0;
	float severityOfStorm <- 0.0;
	float severityOfRainfall <- 0.0;	
	//float perceptionDistance <- 50 #m;
	point velocity <- {0,0};
	point startPoint <- {0,0};
	
	float size <- 0.8 ;
	rgb color <- rgb(141,211,199);
	
	rescuers rescuer;
	rescuersStartingPoints startingPoint;
	
	list<households> householdAtVicinity update: households at_distance resPerceptionDistance;
	list<rescuers> nearestFellowRescuers update: rescuers at_distance(resPerceptionDistance);
	
	
//	reflex calculate_priority 
//	{
//		priority <- self.location distance_to hazard[0];
//	}

	reflex listNearestHouseholds 
	{
		/*
		 * nearestHouseholds contains the list of all the household agents within the perception distance of the rescuer.
		 */
		//list<households> nearestHouseholds <- households at_distance(resPerceptionDistance);
		
		households priority <- households closest_to(self);
		// priority contains the closest households agent from the rescuer agent.	
	}
	
	reflex listNearestFellowRescuers
	{
		//list<rescuers> nearestFellowRescuers <- rescuers at_distance(resPerceptionDistance);
		
		rescuers priorityResc <- rescuers closest_to(self);
	}
	
//	reflex warn when: directed //not alerted 
//	{
//		/**
//		 * Warns households about the impending typhoon.
//		 */
//		if(empty((householdAtVicinity)))// where(each.fire_degree > 0))))
//		{
//			alerted <- false;
//		}
//		else {
//			alerted <- true;
//		}
//	}
	
	reflex warn when: directed
	{
		/**
		 * Look for a nearby destination point when directed by the disaster manager (MDRRMO).
		 */
		rescuersDestinationPoints destination_point;
		//rescuersStartingPoints startPoint;
		evacuationCenters evacuation_center;
		volunteerShelters  volunteer_shelter;
		
		// Destination point at arbitrary near distance
		list<rescuersDestinationPoints> destPoints <- rescuersDestinationPoints at_distance 0#m; 
		
		using topology(road_network) 
		{
			destination_point <- rescuersDestinationPoints with_min_of(each distance_to self);
			
			//evacuation_center <- evacuationCenters with_min_of(each distance_to self);	
			//volunteer_shelter <- volunteerShelters  with_min_of(each distance_to self);		
			startingPoint <- rescuersStartingPoints with_min_of(each distance_to self);	//TO CHECK IF BACK TO STARTING POINT
		}
		
		// Go to the nearest destination point
		do goto target: destination_point on: road_network;	
		
		// Along the way, warn/inform the households that are within the perception distance about 
		// the impending storm and recommend evacuation
		loop hhagent over: householdAtVicinity	
		{	
			hhagent.alerted <- true;
			hhagent._sourceOfEvacuationWarning <- 1.0;
		}
		
		// Once a destination point has been reached by a rescuer, remove that destination point  
		// from the environment so that the rescuer can proceed to other destination points.
		ask destPoints	{	do die;	  }
		
		// If all destination points have been reached, then rescuers should go back to the 
		// nearest starting point.
		if(empty(rescuersDestinationPoints))	
		{	
			do goto target: startPoint on: road_network; //original: startPoint
			
			//CHECK IF THE RESCUER IS BACK TO ITS STARTING POINT
			if(location=startPoint)
			{
				//ask startPoint	{	do die;	  }
				
				//backToStartingPoint <- true;  //RESCUER IS BACK TO ITS STARTING POINT
				//if(rescuersBackToStartingPoint = 15) { rescuersBackToStartingPoint <- 0; }
				rescuersBackToStartingPoint <- rescuersBackToStartingPoint + 1;
				write "rescuersBackToStartingPoint: " + rescuersBackToStartingPoint;
		
			}
		}	
			
	}
	
	//JUST IN CASE; NOT YET NEEDED AS OF THE MOMENT
	action bounding 
	{
			if  (location.x) < xmin			{	velocity <- velocity + {xbounds,0};		} 
			else if (location.x) > xmax 	{	velocity <- velocity - {xbounds,0};		}
			
			if (location.y) < ymin 			{	velocity <- velocity + {0,ybounds};		} 
			else if (location.y) > ymax 	{	velocity <- velocity - {0,ybounds};		}	
	}
	
	
	aspect default 
	{
		draw circle(3#m) color: fullCapacity ? #blue : #green; 
		draw circle(resPerceptionDistance) color: #orange empty: true;	//perception distance
	}
	
	//BEHAVIORS *********************
	//reflex compute riskPerceived(){}
	//fetchHousehold()
	//goToAssignedPost()
	//goToEvacuationCenter()
	//helpFellowRescuer()
	//reportToMDRRMO()
	//computeVitality()
	//computePerceivedRisk()
	//die()
	
	
//	households calculate_priority(list<households> a, float p){
//		households result;
//		int idx;
//		float maxval <-0.0;
//		float val<-0.0;
//		
//		loop obs over: a {
//			float x <- obs distance_to self;
//			float y <- obs distance_to first(hazard);		
//			val <- 1/((x)^p*(y)^(1-p));
//			
//			if(val>maxval){
//				maxval<-val;
//				result<-obs;
//			}
//		}
//		return result;	
//	}
//	
//	action bounding {
//	
//			if  (location.x) < xmin {
//				velocity <- velocity + {xbounds,0};
//			} else if (location.x) > xmax {
//				velocity <- velocity - {xbounds,0};
//			}
//			
//			if (location.y) < ymin {
//				velocity <- velocity + {0,ybounds};
//			} else if (location.y) > ymax {
//				velocity <- velocity - {0,ybounds};
//			}	
//	}

}



species shelterManagers 
{
	// a shelter manager is assigned to an evacuation center
	evacuationCenters evacCenter;
	
	// the capacity of the evacuation center where the shelter manager 
	// is assigned is also its capacity
	//int capacity <- int(evacCenter.evacuationCenterCapacity);	
	
	float size <- 1.0 ;
	
	
	list<households> incomingEvacuees update: households at_distance sheltPerceptionDistance;
	int acceptedEvacuees;
	bool full;
	households evacuee; 
	
	
//	reflex checkCapacity
//	{
//		//write capacity + ": " + acceptedEvacuees;
//		if(acceptedEvacuees < int(evacCenter.evacuationCenterCapacity))		
//		{	
//			//evacuateHouseholdsInside <- households at_distance 1#m;
//			evacuee <- first(incomingEvacuees);
//			do accept_evacuees();
//		}
//		else if(acceptedEvacuees = int(evacCenter.evacuationCenterCapacity))	
//		{	
//			full <- true;
//			evacCenter.full <- true;
//
//			if(availableEvacuationCenters > 0)
//			{
//				availableEvacuationCenters <- availableEvacuationCenters - 1;
//				//write "availableEvacuationCenters:" + availableEvacuationCenters;
//			}		
//		}
//	}
//	
//	action accept_evacuees
//	{
//		ask evacuee 
//		{
//			myself.acceptedEvacuees <- myself.acceptedEvacuees + 1;
//			evacuated_inside <- evacuated_inside + 1;
//			do die;
//		}
//	}
		
	aspect default 
	{
		draw circle(2#m) color:#blue;
		draw circle(sheltPerceptionDistance) color: #purple empty: true;	//perception distance
	}

	
	//BEHAVIORS *************
	//countEvacuees()
	//checkCapacity()
	//disallowEntry()
}

species building 
{
	float houseMaterial <- 0.0;
	float distanceFromHazardSource <- 0.0;
	
	aspect default 
	{
		draw shape color: #gray border: #black;
	}
}

species hazard 
{
	float speed <- 5#m/30#mn;
	
	init 
	{
		catastrophe_date <- current_date + time_before_hazard;
	}
	
//	reflex expand when:catastrophe_date < current_date {
//		shape <- shape buffer (speed);
//		ask households overlapping self {
//			if(self.stay=true){
//					casualties <- casualties + 1; 
//			}
//					
//			self.assigned_rescuer.target <- nil;
//			do die;
//		}
//		
//		ask evacuation_point where (each distance_to self < 2#m) {
//			list<evacuation_point> available_exit <- evacuation_point where (each != self);
//			ask households where (each.safety_point = self) {
//				self.safety_point <- available_exit with_min_of (each distance_to self);
//			}
//			do die;
//		} 
//	}
	
//	aspect default {
//		draw shape color:#blue;
//	}
	aspect default {
		draw (shape + 9) intersection world color: #blue;
	}
}



species road 
{
	int users;
	int capacity <- int(shape.perimeter*8);
	float speed_coeff <- 1.0;
	
	reflex update_weights 
	{
		speed_coeff <- exp(-users/capacity);
		road_weights[self] <- shape.perimeter / speed_coeff;
		users <- 0;
	}
	
	aspect default
	{
		draw shape width: 4#m-(3*speed_coeff)#m color:rgb(55+200*users/capacity,0,0);
	}	
	
 	aspect dummy_1 
 	{
		draw shape rotated_by(90) width: 4#m-(3*speed_coeff)#m color:rgb(55+200*users/capacity,0,0);
	}
}

//species evacuation_point 
//{
//	int count_exit <- 0;
//	
//	action evacue_inhabitant 
//	{
//		count_exit <- count_exit + 1;
//	}
//}

species rescuersStartingPoints
{
	int count_start <- 0;
	bool rescuerIsBackToStartingPoint;  //ADDED
	
	aspect default 
	{
		draw shape + 8 color: #green border: #black;
	}
	
	action gatherRescuers
	{
		count_start <- count_start + 1;
	}
}

species householdsEvacuationPoints
{
	// Species that represents households evacuation points
    // Households that are nearby these evacuation points get evacuated from the hazard.
	
	int count_start <- 0;
	
	aspect default 
	{
		draw shape + 8 color: #violet border: #black;
	}
	
	reflex evacuate_households
	{
		//Evacuates nearby households.
		list<households> evacuateHouseholdsOutside <- households at_distance 0#m; // arbitrary near distance
		
		ask evacuateHouseholdsOutside 
		{
			evacuated_outside <- evacuated_outside + 1;
			do die;
		}
	}
	
	action collectEvacuees
	{
		count_start <- count_start + 1;
	}
}

species evacuationCenters
{
	shelterManagers evacCenterManager;
	string evacuationCenterCapacity;
	int acceptedEvacuees;
	bool full;
	households evacuee; 
	
	//list<households> evacuateHouseholdsInside update: households at_distance 0#m; // arbitrary near distance
	list<households> evacuateHouseholdsInside;
	
	reflex checkCapacity
	{
		//write evacuationCenterCapacity + ": " + acceptedEvacuees;
//		ask self
//		{
		if(acceptedEvacuees < int(evacuationCenterCapacity))		
		{	
			evacuateHouseholdsInside <- households at_distance 0#m;
			evacuee <- first(evacuateHouseholdsInside);
			do accept_evacuees(evacuee);
		}
		else if(acceptedEvacuees = int(evacuationCenterCapacity))	
		{	
			full <- true;

			if(availableEvacuationCenters > 0)
			{
				availableEvacuationCenters <- availableEvacuationCenters - 1;
				write "availableEvacuationCenters:" + availableEvacuationCenters;
			}		
		}
		//}
	}
	
	action accept_evacuees(households evac)
	{
		ask evac 
		{
			myself.acceptedEvacuees <- myself.acceptedEvacuees + 1;
			evacuated_inside <- evacuated_inside + 1;
			do die;
		}
	}
	

	aspect default 
	{
		draw shape color: (acceptedEvacuees < int(evacuationCenterCapacity)) ? #cyan : #red  border: #black;
	}
}

species volunteerShelters
{
	string volunteerShelterCapacity;
	int acceptedVEvacuees;
	bool full;
	
	reflex checkCapacity
	{
		ask self
		{
			if(acceptedVEvacuees = volunteerShelterCapacity)
			{
				full <- true;
			}
		}
	}
	
	aspect default 
	{
		draw shape color: #yellowgreen border: #black;
	}
}

species rescuersDestinationPoints
{
	bool reached;
	
	aspect default
	{
		draw shape color: #orange;
	}
}

species floodproneAreas
{
	string floodRisk;
	
	aspect default 
	{	
		draw shape color: floodRisk = "veryHighSusceptibility" ? rgb(179,0,0) : rgb(227,74,51);
		//draw shape color: floodRisk = "veryHighSusceptibility" ? rgb(231,41,138) : rgb(255,255,153);
	}
}


experiment Displays_typhoon_evacuation_behavior type: gui 
{
	//parameter "Direct Rescuers" var: directed <- true;
	parameter "Storm Severity"  var: stormSeverity init:1 min:1 max:2 step:1;
	parameter "Rainfall Classification"  var: rainfallSeverity init:"Yellow" among:["Red","Orange","Yellow"];// category: "Rainfall Classification";
	parameter "Time of Day"  var: timeOfDay init:"Daytime" among:["Daytime","Night time"];
	//parameter "Time before hazard" var:time_before_hazard init:2#h min:2#h max:4#h;
	
	//parameter "Households: " var: nb_households_init min: 20 max: 577 category: "Number of Agents" ;
	parameter "Rescuers: " var: nb_rescuers_init   min: 15 max: 21 step: 1 category: "Number of Agents" ;
	parameter "Shelter Managers: " var: nb_shelterManagers_init min: 1 max: 4 step: 1 category: "Number of Agents" ;
	
//	parameter "Household: " var: hhPerceptionDistance min: 50#m max: 100#m category: "Perception Distance";
//	parameter "Rescuer: " var: resPerceptionDistance min: 50#m max: 100#m category: "Perception Distance";
//	parameter "Shelter Manager: " var: sheltPerceptionDistance min: 50#m max: 100#m category: "Perception Distance";
	
	parameter "Weight for CDM: " var: weight_CDM init: 0.4 min: 0.1 max: 0.9 step: 0.1 category: "Weights";
	parameter "Weight for HRF: " var: weight_HRF init: 0.3 min: 0.1 max: 0.9 step: 0.1 category: "Weights";
	parameter "Weight for CRF: " var: weight_CRF init: 0.3 min: 0.1 max: 0.9 step: 0.1 category: "Weights";
	//parameter "Initial Probability of Cooperation" var:init_probability_cooperation init:0.5 min:0.0 max:1.0;
	
	output 
	{
		display main_display type:opengl 
		{ 
			species floodproneAreas;
			species road;
			species rescuersDestinationPoints;
			species rescuersStartingPoints;
			species householdsEvacuationPoints;
			//species evacuation_point;
			species hazard;
			species building;
			species evacuationCenters;
			species volunteerShelters;
			species households;
			species rescuers;
			species shelterManagers;
		}
		
		display info_display type:opengl 
		{ 
			species floodproneAreas;
			species road;
			species rescuersDestinationPoints;
			species rescuersStartingPoints;
			species householdsEvacuationPoints;
			//species evacuation_point;
			species hazard;
			species building;
			
			species evacuationCenters;
			species volunteerShelters;
			species households;
			species rescuers;
			species shelterManagers;
		}
		
		display chart_pie {
			chart "Evacuation Data" type:pie{
				data "Evacuated inside barangay" value: evacuated_inside;
				data "Evacuated outside barangay " value: evacuated_outside;		
				data "Did not evacuate " value: 570 - evacuated_inside + evacuated_outside;										
			}
		}
						
		monitor "Evacuated: Inside Barangay" value: evacuated_inside ;
		monitor "Evacuated: Outside Barangay" value: evacuated_outside ;
		monitor "Total Evacuees" value: evacuated_inside + evacuated_outside ;
		
		
			
	}
	
}


experiment batchExperiment type: batch repeat: 2 keep_seed: true until: empty(rescuersDestinationPoints) or 
(weight_CDM+weight_HRF+weight_CRF > 1.0) or (weight_CDM+weight_HRF+weight_CRF < 1.0){
	//and STOP the simulation when the rescuers are already back to their starting points and no households are moving anymore.
	float seedValue <- 10.0;
	float seed <- seedValue ; // force the value of the seed .
	
	parameter "Storm Severity"  var: stormSeverity init:2 min:2 max:2 step:1;
	parameter "Rainfall Classification"  var: rainfallSeverity init:"Yellow" among:["Yellow","Orange","Red"];// category: "Rainfall Classification";
	parameter "Time of Day"  var: timeOfDay init:"Daytime" among:["Daytime","Night time"];
	
	parameter "Rescuers: " var: nb_rescuers_init   min: 15 max: 15 step: 1 category: "Number of Agents" ;
	parameter "Shelter Managers: " var: nb_shelterManagers_init min: 4 max: 4 step: 1 category: "Number of Agents" ;
	
	parameter "Weight for CDM: " var: weight_CDM  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.3
	parameter "Weight for HRF: " var: weight_HRF  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.4
	parameter "Weight for CRF: " var: weight_CRF  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.3
	
//	parameter "Number of Rescuers" var:nb_of_rescuers  min: 10 max: 20 step:5;
//	parameter "Rescue Strategy" var:the_rescue_strategy init:"DEFAULT" among:["PRIORITY","DEFAULT"] category: "Strategy";
	//parameter "Risk Distance Ratio" var:the_ratio init:0.5 min: 0 max:1 step:0.1 category: "Strategy";
	//parameter "Nb people" var: nb_of_people <- 5000 min: 1000 max: 10000 step: 1000 category: "Init";
	
	method exhaustive;	
	
	reflex results 
	{
		if(weight_CDM+weight_HRF+weight_CRF = 1.0)
		{
			save [weight_CDM, 
				weight_HRF,
				weight_CRF, 
				stormSeverity,
				rainfallSeverity,
				timeOfDay,
				
				mean(simulations collect each.evacuated_inside), 
				mean(simulations collect each.evacuated_outside)]  
				type: "csv" to: "resultsAgregated.csv" rewrite: true;
		
			ask simulations 
			{
				save [weight_CDM, 
					weight_HRF,
					weight_CRF, 
					stormSeverity,
					rainfallSeverity,
					timeOfDay, 
					self.evacuated_inside, 
					self.evacuated_outside] 
					type: "csv" to: "results.csv" rewrite: true;	
			}			
		}
	}
	
	permanent 
	{
		display disply 
		{
			chart "evacuated" type: series 
			{
//				data "casualties" value: casualties;
//				if(weight_CDM+weight_HRF+weight_CRF = 1.0)
//				{
					data "evacuated households" value: evacuated_inside + evacuated_outside;
//				}
			}
		}
	}	
}

experiment batchTest type: batch repeat: 2 keep_seed: true until: empty(rescuersDestinationPoints) or 
(weight_CDM+weight_HRF+weight_CRF > 1.0) or (weight_CDM+weight_HRF+weight_CRF < 1.0) {
	//and STOP the simulation when the rescuers are already back to their starting points and no households are moving anymore.
	
	float seedValue <- 10.0;
	float seed <- seedValue ; // force the value of the seed .
	
	parameter "Storm Severity"  var: stormSeverity init:2 min:2 max:2 step:1;
	parameter "Rainfall Classification"  var: rainfallSeverity init:"Yellow" among:["Yellow","Orange","Red"];// category: "Rainfall Classification";
	parameter "Time of Day"  var: timeOfDay init:"Daytime" among:["Daytime","Night time"];
	
	parameter "Rescuers: " var: nb_rescuers_init   min: 15 max: 15 step: 1 category: "Number of Agents" ;
	parameter "Shelter Managers: " var: nb_shelterManagers_init min: 4 max: 4 step: 1 category: "Number of Agents" ;
	
	parameter "Weight for CDM: " var: weight_CDM  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.3
	parameter "Weight for HRF: " var: weight_HRF  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.4
	parameter "Weight for CRF: " var: weight_CRF  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.3
	
	method exhaustive;	
	
	reflex results 
	{
		if(weight_CDM+weight_HRF+weight_CRF = 1.0)
		{
			save [weight_CDM, 
				weight_HRF,
				weight_CRF, 
				stormSeverity,
				rainfallSeverity,
				timeOfDay,
				
				mean(simulations collect each.evacuated_inside), 
				mean(simulations collect each.evacuated_outside)]  
				type: "csv" to: "resultsAgregated.csv" rewrite: false;
		
			ask simulations 
			{
				save [weight_CDM, 
					weight_HRF,
					weight_CRF, 
					stormSeverity,
					rainfallSeverity,
					timeOfDay, 
					self.evacuated_inside, 
					self.evacuated_outside] 
					type: "csv" to: "results.csv" rewrite: false;	
			}			
		}
	}
	
	permanent 
	{
		display disply 
		{
			chart "evacuated" type: series 
			{
//				data "casualties" value: casualties;
//				if(weight_CDM+weight_HRF+weight_CRF = 1.0)
//				{
					data "evacuated households" value: evacuated_inside + evacuated_outside;
//				}
			}
		}
	}	
}


//THIS IS THE WORKING BATCH EXPERIMENT
experiment batchT type: batch repeat: 5 keep_seed: true until: (rescuersBackToStartingPoint >= nb_rescuers_init) or 
(weight_CDM+weight_HRF+weight_CRF > 1.0) or (weight_CDM+weight_HRF+weight_CRF < 1.0) {
	//STOP the simulation when the rescuers are already back to their starting points and no households are moving anymore.
	
	float seedValue <- 10.0;
	float seed <- seedValue ; // force the value of the seed .
	
	parameter "Storm Severity"  var: stormSeverity init:1 min:1 max:2 step:1;
	parameter "Rainfall Classification"  var: rainfallSeverity init:"Yellow" among:["Yellow","Orange","Red"];// category: "Rainfall Classification";
	parameter "Time of Day"  var: timeOfDay init:"Daytime" among:["Daytime","Night time"];
	
	parameter "Rescuers: " var: nb_rescuers_init   min: 15 max: 15 step: 1 category: "Number of Agents" ;
	parameter "Shelter Managers: " var: nb_shelterManagers_init min: 4 max: 4 step: 1 category: "Number of Agents" ;
	
	parameter "Weight for CDM: " var: weight_CDM  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.3
	parameter "Weight for HRF: " var: weight_HRF  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.4
	parameter "Weight for CRF: " var: weight_CRF  min: 0.1 max: 0.8 step: 0.1 category: "Weights"; //init: 0.3
	
	method exhaustive;	
	
	reflex results 
	{
		if(weight_CDM+weight_HRF+weight_CRF = 1.0)
		{
			//rescuersBackToStartingPoint <- 0;
			save [weight_CDM, 
				weight_HRF,
				weight_CRF, 
				stormSeverity,
				rainfallSeverity,
				timeOfDay,
				
				mean(simulations collect (each.evacuated_inside+each.evacuated_outside))]
				//mean(simulations collect each.evacuated_outside)]  
				type: "csv" to: "resultsAgregated.csv" rewrite: false;
		
			ask simulations 
			{
				save [weight_CDM, 
					weight_HRF,
					weight_CRF, 
					stormSeverity,
					rainfallSeverity,
					timeOfDay, 
					self.evacuated_inside+self.evacuated_outside] 
					type: "csv" to: "results.csv" rewrite: false;	
			}			
		}
	}
	
	permanent 
	{
		display disply 
		{
			chart "evacuated" type: series 
			{
//				data "casualties" value: casualties;
//				if(weight_CDM+weight_HRF+weight_CRF = 1.0)
//				{
					data "evacuated households" value: evacuated_inside + evacuated_outside;
//				}
			}
		}
	}	
}










































////Batch experiment to find the best way to assign Decision Factor weights using exhaustive method
//experiment Batch type: batch repeat: 100 keep_seed: true until: (food_gathered = food_placed) or (time > 1000) 
//{
//	parameter 'Size of the grid:' var: gridsize init: 75 unit: 'width and height';
//	parameter 'Number:' var: ants_number <- 10 among: [10, 20, 50] unit: 'ants';
//	parameter 'Evaporation:' var: evaporation_per_cycle <- 0.1 among: [0.1, 0.5, 2.0, 10.0] unit: 'units every cycle';
//	parameter 'Diffusion:' var: diffusion_rate min: 0.1 max: 1.0 unit: 'rate every cycle (1.0 means 100%)' step: 0.2;
//	method exhaustive maximize: food_gathered;
//	permanent 
//	{
//		display Comparison background: #white 
//		{
//			chart "Food Gathered" type: series 
//			{
//				data "Min" value: min(ants_model collect each.food_gathered) style: spline color: #darkgreen;
//				data "Max" value: max(ants_model collect each.food_gathered) style: spline color: #red;
//			}
//		}
//	}
//}

