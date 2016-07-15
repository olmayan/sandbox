#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QGraphicsScene>
#include "graphicsview.h"
#include <QGraphicsSvgItem>
#include <QParallelAnimationGroup>
#include <QSequentialAnimationGroup>
#include <QMouseEvent>
#include <QMenu>
#include "field.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

public slots:
    void lock();
    void unlock();
	void removeLines();
	void deleteGraphicItems();
	void growBalls();
	void showNextBalls();
    void newGame();

signals:
    
protected:
    void resizeEvent(QResizeEvent *);

private:
	Field *pGameField, *pNextBalls, *pDeletionMap;
    QGraphicsScene *graphicsScene;
    GraphicsView *graphicsView;
    QVector<QGraphicsSvgItem> tiles;
    QGraphicsSvgItem *selectItem;
    QPoint selection;
    QVector<QGraphicsSvgItem *> balls;
    QVector<QGraphicsSvgItem *> nextBalls;
    QAnimationGroup *animGroup;

    QMenu *gameMenu;
    QAction *gameNew;
    QAction *gameNewLines;
    QAction *gameNewSquares;
    QAction *gameNewBlocks;
    QAction *gameExit;
    QMenu *helpMenu;
    QAction *helpAboutQt;

    bool locked;
	int score;
    GameType gameType;

private slots:
    void processMouseEvent(QMouseEvent *);
};

#endif // MAINWINDOW_H
