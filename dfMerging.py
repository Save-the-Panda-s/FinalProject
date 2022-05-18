# Adding a new binary columns for Team 1 Win or Loss
conditions = [
    results['Team1Score'] > results['Team2Score'],
    results['Team1Score'] < results['Team2Score']
]

choices=[1,0]

results['Team1Win'] = np.select(conditions, choices, 1)

# Mergings the results and stats datasets, adding a prefix depending on which teams stats are being used
df = results.merge(stats.add_prefix('Team1'), how='left', left_on=['Team1'], 
                   right_on=['Team1TEAM']).drop(['Team1TEAM'],
                                                axis=1).merge(stats.add_prefix('Team2'), 
                                                                            how='left', left_on=['Team2'], 
                                                                            right_on=['Team2TEAM']).drop(['Team2TEAM'],axis=1)
df.to_csv(r'/path/filename.csv', index = False, header=True)