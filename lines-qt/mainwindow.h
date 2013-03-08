#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QGraphicsScene>
#include "graphicsview.h"
#include <QGraphicsSvgItem>
#include <QStateMachine>
#include <QMouseEvent>
#include "field.h"


class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

signals:
    
protected:
    //void paintEvent(QPaintEvent *);
    void resizeEvent(QResizeEvent *);
    //void mouseReleaseEvent(QMouseEvent *);

private:
    Field *pGameField, *pNextBalls;
    QGraphicsScene *graphicsScene;
    GraphicsView *graphicsView;
    QGraphicsSvgItem *selectItem;
    QPoint selection;
    QVector<QGraphicsSvgItem *> balls;
    QVector<QGraphicsSvgItem *> pathMarkers;
    QStateMachine stateMachine;

private slots:
    void processMouseEvent(QMouseEvent *);
};

#endif // MAINWINDOW_H
